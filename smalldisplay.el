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
(require 'xcb)

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

(defun smalldisplay-svg-multi-line-text (svg texts &rest args)
  "Add TEXT to SVG."
  (let ((a (svg--arguments svg args)))
    (svg--append
     svg
     (apply
      'dom-node 'text `(,@a)
      (cl-loop for text in texts
	       collect (dom-node 'tspan `((dy . "1.0em")
					  (x . ,(cdr (assoc 'x a))))
				 (svg--encode-text
				  ;; To ensure that spaces survive the
				  ;; SVG machinery (especially around
				  ;; entities like &amp;), make them
				  ;; into non-breaking spaces.
				  (replace-regexp-in-string
				   (string 32) (string 160) text))))))))

(defun smalldisplay--temp ()
  (with-temp-buffer
    (call-process "get-temperatures" nil t)
    (loop for temp in (split-string (buffer-string) nil t)
	  collect (format "%.1f°C" (string-to-number temp)))))

(defun smalldisplay--track ()
  (let ((track (jukebox-tokenize-path (smalldisplay--current))))
    ;; If the album and song name is the same, then drop the track
    ;; name.
    (cond
     ((or (equal (nth 1 track) (nth 2 track))
	  (and (> (length (nth 1 track)) 6)
	       (zerop (or (search (nth 1 track) (nth 2 track)) -1))))
      (list (car track) (caddr track)))
     ((equal (nth 0 track) (nth 1 track))
      (list (car track) (nth 2 track)))
     (t
      track))))

(defvar smalldisplay-current-track "/music/tmp/.amp.current")

(defun smalldisplay--current ()
  (with-temp-buffer
    (insert-file-contents smalldisplay-current-track)
    (buffer-substring (point-min) (1- (point-max)))))

(defvar smalldisplay-current-track-last nil)

(defun smalldisplay-track-changed-p ()
  (let ((current (ignore-errors
		   (smalldisplay--current))))
    (if (not current)
	nil
      (let ((new (list (ignore-errors
			 (file-attribute-modification-time
			  (file-attributes
			   smalldisplay-current-track)))
		       current)))
	(if (equal new smalldisplay-current-track-last)
	    nil
	  (setq smalldisplay-current-track-last new)
	  t))))) 

(defmacro smalldisplay-loop (&rest body)
  `(loop
    (condition-case err
	(progn
	  ,@body)
      (error (message "%s" err)
	     (sleep-for 10)))))

(defun smalldisplay-loop-stories ()
  (smalldisplay-loop
   (loop for i from 0
	 when (or (smalldisplay-track-changed-p)
		  (zerop (mod i 60)))
	 do (smalldisplay-stories)
	 do (sleep-for 1))))

(defun smalldisplay-loop-quimbies ()
  (let ((i 0))
  (smalldisplay-loop
   (loop
    (when (smalldisplay-track-changed-p)
      (smalldisplay-quimbies))
    (when (zerop (mod (incf i) 100))
      (smalldisplay-frame))
    (sleep-for 1)))))

(defun smalldisplay-mpv-id ()
  (loop for (id . name) in (smalldisplay-list-windows)
	when (and name
		  (string-match "mpv" name))
	return id))

(defun smalldisplay-loop-potato ()
  (let (mpv new-mpv)
    (smalldisplay-loop
     (loop for i from 0
	   when (or
		 (smalldisplay-track-changed-p)
		 (not (equal (setq new-mpv (smalldisplay-mpv-id)) mpv))
		 (zerop (mod i 30)))
	   do (smalldisplay-potato)
	   (setq mpv new-mpv)
	   do (sleep-for 1)))))

(defun smalldisplay-stories ()
  (message (format-time-string "%H:%M:%S Making"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((track (smalldisplay--track)))
      (insert (smalldisplay '(800 . 480)
			    `((bottom-left ,(if (= (length track) 3)
						310
					      370)
					   50
					   ,track)
			      (top-right 0 50 ,(smalldisplay--temp)))
			    (expand-file-name
			     "sleeve.jpg" (file-name-directory
					   (smalldisplay--current)))))
      (call-process-region (point-min) (point-max)
			   "xloadimage" nil nil nil
			   "-display" ":1" "-onroot" "-gamma" "2" "stdin"))))

(require 'seq)

(defun smalldisplay-frame ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((files (seq-remove
		  (lambda (file)
		    (string-match "[0-9]+x[0-9]+\\|scaled" file))
		  (directory-files-recursively
		   "/var/tmp/uploads" "shot.*[.]jpg\\'")))
	   (file (nth (random (length files)) files)
		 ;;(nth 1300 files)
		 ))
      (call-process "convert" nil nil nil
		    "-trim" "-fuzz" "10%"
		    file "/tmp/trim.jpg")
      (call-process "sharp" nil nil nil
		    "-m" "1" "-f" "10"
		    "/tmp/trim.jpg" "/tmp/sharp.jpg"))
    (insert-file-contents-literally "/tmp/sharp.jpg")
    (call-process-region (point-min) (point-max)
			 "convert"
			 t (current-buffer) nil
			 "jpg:-"
			 "-resize" "1200x825^"
			 "-gravity" "Center"
			 "-extent" "1200x825"
			 "-level" "0%,80%"
			 "-contrast-stretch" "0.0x5.0%"
			 "-colorspace" "gray"
			 ;;"-noise" "5" "-median" "5" "-unsharp" "5"
			 ;;"-posterize" "16"
			 ;;"-auto-level"
			 "/tmp/sleeve-stretch.jpg")
    (let ((track (smalldisplay--track)))
      (insert (smalldisplay '(1200 . 825)
			    `((bottom-right
			       600 200
			       ,(list (string-remove-suffix
				       "C"
				       (cadr (smalldisplay--temp))))))
			    "/tmp/sleeve-stretch.jpg"))
      (write-region (point-min) (point-max) "/tmp/a.png")
      (call-process-region (point-min) (point-max)
			   "convert"
			   t (current-buffer) nil
			   "png:-"
			   ;;"-rotate" "180"
			   "-depth" "4"
			   "pgm:-")
      (write-region (point-min) (point-max) "/tmp/a.pgm")
      (goto-char (point-min))
      (forward-line 3)
      ;; Remove the PGM header.
      (delete-region (point-min) (point))
      (while (not (eobp))
	(insert (+ (* (char-after) 1)
		   (* (char-after (1+ (point))) 16)))
	(delete-region (point) (+ (point) 2)))
      (call-process-region (point-min) (point-max)
			   "pigz"
			   t (current-buffer) nil
			   "-zc")
      (write-region (point-min) (point-max) "~/tmp/frame/image-temp.rawz")
      (rename-file "~/tmp/frame/image-temp.rawz"
		   "~/tmp/frame/image.rawz" t))))



(defun smalldisplay-quimbies ()
  (message (format-time-string "%H:%M:%S Making"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (smalldisplay '(1280 . 800)
			  `((top-left -30 260 ,(smalldisplay--track)))
			  (expand-file-name
			   "sleeve.jpg" (file-name-directory
					 (smalldisplay--current)))))
    (call-process-region (point-min) (point-max)
			 "xloadimage" nil nil nil
			 "-display" ":1" "-onroot" "-gamma" "2" "stdin")))

(defvar smalldisplay-displayer nil)

(defun smalldisplay-potato (&optional debug)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (smalldisplay-potato-1
	     '(1280 . 800)
	     `((top-right 0 80 ,(list (format-time-string "%H:%M")
				      (cadr (smalldisplay--temp))))
	       (bottom-right 690 30 ,(smalldisplay--track)))
	     (smalldisplay-smooth
	      (loop for point in (smalldisplay-rain)
		    collect (cons (* (car point) (/ 1280.0 24))
				  (- 803 (* (cdr point) 130)))))))
    (write-region (point-min) (point-max) "/tmp/a.png")
    (if debug
	(call-process-region (point-min) (point-max)
			     "feh" nil nil nil "-ZF" "/tmp/a.png")
      (let ((prev smalldisplay-displayer))
	(setq smalldisplay-displayer
	      (start-process "qiv" nil "/usr/src/qiv-2.2.4/qiv"
			     "-p" "--display" ":1" "/tmp/a.png"))
	(when prev
	  (sleep-for 0.1)
	  (delete-process prev))))))

(defun smalldisplay-potato-1 (size texts rain)
  (let ((svg (svg-create (car size) (cdr size)
			 :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (when nil
    (svg-rectangle svg 0 0 (car size) (cdr size)
		   :fill "#000001"))
    (smalldisplay-svg-path
     svg
     :d (smalldisplay-path rain)
     :stroke-width 7
     :fill "none"
     :stroke "white")
    (smalldisplay-text svg size texts)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (if t
	  (call-process-region (point-min) (point-max)
			       "convert"
			       t (list (current-buffer) nil)
			       nil
			       ;;"-transparent" "#000001"
			       "-background" "transparent"
			       ;;"+antialias"
			       "svg:-" "png:-")
	(write-region (point-min) (point-max) "/tmp/b.svg")
	(call-process "inkscape" nil nil nil "-z" "/tmp/b.svg"
		      "-e" "/tmp/b.png")
	(erase-buffer)
	(insert-file-contents-literally "/tmp/b.png"))
      (buffer-string))))

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


(defvar smalldisplay-rain nil)
(defvar smalldisplay-rain-count 0)

(defun smalldisplay-rain ()
  (if (and smalldisplay-rain
	   (not (zerop (mod (incf smalldisplay-rain-count) 60))))
    ;; Serve the cached rain values usually.
    smalldisplay-rain
    (let ((rain
	   (with-current-buffer
	       (url-retrieve-synchronously
		"http://www.yr.no/stad/Noreg/Oslo/Oslo/Oslo/varsel_time_for_time.xml"
		nil nil 30)
	     (goto-char (point-min))
	     (when (search-forward "\n\n" nil t)
	       (loop for elem in (dom-by-tag
				  (libxml-parse-xml-region (point) (point-max))
				  'precipitation)
		     for i from 0
		     collect (cons i (string-to-number
				      (dom-attr elem 'value))))))))
      (setq smalldisplay-rain rain)
      rain)))

(defun smalldisplay-line (a b)
  (let ((length-x (- (car b) (car a)))
	(length-y (- (cdr b) (cdr a))))
    (list :length (sqrt (+ (expt length-x 2) (expt length-y 2)))
	  :angle (atan length-y length-x))))

(defun smalldisplay-control-point (current previous next reverse)
  (let* ((previous (or previous current))
	 (next (or next current))
	 (line (smalldisplay-line previous next))
	 (angle (+ (getf line :angle)
		   (if reverse
		       pi
		     0)))
	 (smoothing 0.2)
	 (length (* (getf line :length) smoothing)))
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

(defun smalldisplay-svg-path (svg &rest args)
  "Add TEXT to SVG."
  (svg--append svg (dom-node 'path `(,@(svg--arguments svg args)))))

(defun smalldisplay-path (points)
  (mapconcat
   #'identity
   (loop for point in points
	 for i from 0
	 collect (if (zerop i)
		     (format "M %s,%s" (car point) (cdr point))
		   (smalldisplay-bezier i points)))
   " "))

(defun smalldisplay-text (svg size texts &rest args)
  (loop for (position y font-size strings . no-border) in texts
	do (if no-border
	       (apply
		'smalldisplay-svg-multi-line-text
		svg strings
		:text-anchor
		(if (memq position '(top-right bottom-right))
		    "end"
		  "start")
		:x (if (memq position '(top-right bottom-right))
		       (- (car size) 20)
		     20)
		:y y
		:font-size font-size
		:font-weight "bold"
		:fill "white"
		:font-family "futura"
		args)
	     (loop for stroke in (list (max (/ font-size 16) 2) 1)
		   do (apply
		       'smalldisplay-svg-multi-line-text
		       svg strings
		       :text-anchor
		       (if (memq position '(top-right bottom-right))
			   "end"
			 "start")
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
		       :font-family "futura"
		       args)))))

(defun smalldisplay-list-windows (&optional display)
  (let* ((x (xcb:connect (or display ":1")))
	 (root (slot-value (car (slot-value (xcb:get-setup x) 'roots))
                           'root))
	 (tree (xcb:+request-unchecked+reply x
                   (make-instance 'xcb:QueryTree
                                  :window root))))
    (prog1
	(loop for child in (slot-value tree 'children)
	      collect (cons
		       child
		       (cadr
			;; We get back a string (well, vector of
			;; characters) that contains two data points:
			;; First the window class (as the property
			;; name says), and then the name of the
			;; window.  The two parts are terminated by a
			;; NUL character each.
			(split-string
			 (coerce
			  (slot-value
			   (xcb:+request-unchecked+reply x
			       (make-instance 'xcb:GetProperty
					      :delete 0
					      :window child
					      :type xcb:Atom:STRING
					      :property xcb:Atom:WM_CLASS
					      :long-length 256
					      :long-offset 0))
			   'value)
			  'string)
			 (string 0)))))
      (xcb:disconnect x))))

(provide 'smalldisplay)

;;; smalldisplay.el ends here
