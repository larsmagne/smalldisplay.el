;;; smalldisplay.el --- compose images to display on small displays -*- lexical-binding: t -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions, processes

;; smalldisplay.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; smalldisplay.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; # apt install xloadimage

;;; Code:

(require 'cl)
(require 'svg)
(require 'jukebox)

(defun smalldisplay-image-size (file)
  (with-temp-buffer
    (call-process "identify" nil (current-buffer) nil
		  "-format" "%wx%h" file)
    (let ((size (split-string (buffer-string) "x")))
      (cons (string-to-number (car size))
	    (string-to-number (cadr size))))))

(defun smalldisplay (size texts &optional image)
  (let ((svg (svg-create (car size) (cdr size)
			 :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (when image
      (let ((image-size (smalldisplay-image-size image))
	    ratio)
	;; Ensure that the image fits on the screen by scaling up/down.
	(setq ratio (/ (* (car size) 1.0) (car image-size)))
	(when (< (* ratio (cdr image-size))
		 (cdr size))
	  (setq ratio (/ (* (cdr size) 1.0) (cdr image-size))))
	(svg-embed svg image
		   (if (string-match "jpg$" image)
		       "image/jpeg"
		     (format "image/%s"
			     (car (last (split-string image "[.]")))))
		   nil
		   :width (* (car image-size) ratio)
		   :height (* (cdr image-size) ratio)
		   ;; Show the center part of the image.
		   :x (- (/ (- (* ratio (car image-size)) (car size)) 2))
		   :y (- (/ (- (* ratio (cdr image-size)) (cdr size)) 2)))))
    (smalldisplay-text svg size texts)      
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (call-process-region (point-min) (point-max) "convert"
			   t (current-buffer)
			   nil "svg:-" "png:-")
      (buffer-string))))

(defun svg-multi-line-text (svg texts &rest args)
  "Add TEXT to SVG."
  (let ((a (svg--arguments svg args)))
    (svg--append
     svg
     (apply
      'dom-node 'text `(,@a)
      (cl-loop for text in texts
	       collect (dom-node 'tspan `((dy . "1.0em")
					  (x . ,(cdr (assoc 'x a))))
				 (svg--encode-text text)))))))

(defun smalldisplay--temp ()
  (with-temp-buffer
    (insert-file-contents "~/jukebox/tex/tempdata.data")
    (split-string (buffer-string) "[\\\\\n]" t)))

(defun smalldisplay--track ()
  (jukebox-tokenize-path (smalldisplay--current)))

(defvar smalldisplay-current-track "/music/tmp/.amp.current")

(defun smalldisplay--current ()
  (with-temp-buffer
    (insert-file-contents smalldisplay-current-track)
    (buffer-substring (point-min) (1- (point-max)))))

(defun smalldisplay-loop-stories ()
  (let ((timestamp nil)
	new) 
    (loop for i from 0
	  when (or
		(not (equal (setq new (file-attribute-modification-time
				       (file-attributes
					smalldisplay-current-track)))
			    timestamp))
		(zerop (mod i 60)))
	  do (smalldisplay-stories)
	  do (sleep-for 1))))

(defun smalldisplay-stories ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (smalldisplay '(800 . 480)
			  `((bottom-left 310 50 ,(smalldisplay--track))
			    (top-right 0 50 ,(smalldisplay--temp)))
			  (expand-file-name
			   "sleeve.jpg" (file-name-directory
					 (smalldisplay--current)))))
    (call-process-region (point-min) (point-max)
			 "xloadimage" nil nil nil
			 "-display" ":1" "-onroot" "-gamma" "2" "stdin")))

(defun smalldisplay-quimbies ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (smalldisplay '(1200 . 800)
			  `((top-left 0 150 ,(smalldisplay--track)))
			  (expand-file-name
			   "sleeve.jpg" (file-name-directory
					 (smalldisplay--current)))))
    (call-process-region (point-min) (point-max)
			 "xloadimage" nil nil nil
			 "-display" ":1" "-onroot" "-gamma" "2" "stdin")))

(defun smalldisplay-potato ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (smalldisplay-potato-1
	     '(1024 . 600)
	     `((top-right 0 70 ,(list (format-time-string "%H:%M")
				      (car (smalldisplay--temp))))
	       (bottom-right 520 20 ,(smalldisplay--track)))
	     (smalldisplay-smooth
	      (loop for point in (smalldisplay-rain)
		    collect (cons (* (car point) (/ 1024.0 24))
				  (- 600 (* (cdr point) 130)))))))
    (write-region (point-min) (point-max) "/tmp/a.png")
    (call-process "qiv" nil nil nil "/tmp/a.png")))

(defun smalldisplay-smooth (points)
  (let ((acc 0)
	(length 4))
    (dotimes (i length)
      (incf acc (cdr (elt points i))))
    (loop for i from length upto (+ 24 length)
	  collect (cons (car (elt points (- i (/ length 2))))
			(prog2
			    (incf acc (cdr (elt points i)))
			    (/ (* acc 1.0) (1+ length))
			  (decf acc (cdr (elt points (- i length)))))))))

(defun smalldisplay-potato-1 (size texts rain)
  (let ((svg (svg-create (car size) (cdr size)
			 :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-path svg
	      :d (smalldisplay-path rain)
	      :stroke-width 7
	      :fill "none"
	      :stroke "white")
    (smalldisplay-text svg size texts)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (call-process-region (point-min) (point-max) "convert"
			   t (list (current-buffer) nil)
			   nil "-background" "transparent"
			   "svg:-" "png:-")
      (buffer-string))))

(defun smalldisplay-rain ()
  (with-current-buffer
      (url-retrieve-synchronously "http://www.yr.no/stad/Noreg/Oslo/Oslo/Oslo/varsel_time_for_time.xml")
    (goto-char (point-min))
    (when (search-forward "\n\n" nil t)
      (loop for elem in (dom-by-tag
			 (libxml-parse-xml-region (point) (point-max))
			 'precipitation)
	    for i from 0
	    collect (cons i (string-to-number (dom-attr elem 'value)))))))

(defun smalldisplay-line (a b)
  (let ((length-x (- (car b) (car a)))
	(length-y (- (cdr b) (cdr a))))
    (list :length (sqrt (+ (expt length-x 2) (expt length-y 2)))
	  :angle (atan length-y length-x))))

(defun smalldisplay-control-point (current previous next reverse)
  (let* ((p (or previous current))
	 (n (or next current))
	 (o (smalldisplay-line p n))
	 (angle (+ (getf o :angle)
		   (if reverse
		       pi
		     0)))
	 (smoothing 0.2)
	 (length (* (getf o :length) smoothing)))
    (cons (+ (car current) (* (cos angle) length))
	  (+ (cdr current) (* (sin angle) length)))))

(defun smalldisplay-bezier (i points)
  (let ((cps (smalldisplay-control-point (elt points (- i 1))
					 (elt points (- i 2))
					 (elt points i)
					 nil))
	(cpe (smalldisplay-control-point (elt points i)
					 (elt points (1- i))
					 (elt points (1+ i))
					 t)))
    (format "C %s,%s %s,%s %s,%s"
	    (car cps) (cdr cps)
	    (car cpe) (cdr cpe)
	    (car (elt points i)) (cdr (elt points i)))))

(defun svg-path (svg &rest args)
  "Add TEXT to SVG."
  (svg--append
   svg
   (dom-node
    'path
    `(,@(svg--arguments svg args)))))

(defun smalldisplay-path (points)
  (mapconcat
   #'identity
   (loop for point in points
	 for i from 0
	 collect (if (zerop i)
		     (format "M %s,%s" (car point) (cdr point))
		   (smalldisplay-bezier i points)))
   " "))

(defun smalldisplay-text (svg size texts)
  (loop for (position y font-size strings) in texts
	do (loop for stroke in (list (/ font-size 16) 1)
		 do (svg-multi-line-text
		     svg strings
		     :text-anchor
		     (if (memq position '(top-right bottom-right))
			 "right"
		       "left")
		     :x (if (memq position '(top-right bottom-right))
			    (- (car size) 20)
			  20)
		     :y (or y
			    (if (memq position '(bottom-left bottom-right))
				(- (cdr size) (* (length texts) 100) 20)
			      20))
		     :font-size font-size
		     :stroke "black"
		     :stroke-width (format "%dpx" stroke)
		     :font-weight "bold"
		     :fill "white"
		     :font-family "futura"))))

(provide 'smalldisplay)

;;; smalldisplay.el ends here
