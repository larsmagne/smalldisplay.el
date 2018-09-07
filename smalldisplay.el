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

;;; Code:

(require 'cl)
(require 'svg)

(defun smalldisplay (size texts &optional image)
  (let ((svg (svg-create (car size) (cdr size)
			 :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (when image
      (let ((image-size (image-size (create-image image) t))
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
    (loop for (position y strings) in texts
	  do (svg-multi-line-text
	      svg strings
	      :text-anchor (if (memq position '(top-right bottom-right))
			       "right"
			     "left")
	      :x (if (memq position '(top-right bottom-right))
		     (- (car size) 20)
		   20)
	      :y (or y
		     (if (memq position '(bottom-left bottom-right))
			 (- (cdr size) (* (length texts) 100) 20)
		       20))
	      :font-size 50
	      :stroke "black"
	      :stroke-width "2px"
	      :font-weight "bold"
	      :fill "white"
	      :font-family "futura"))
      
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
  (with-temp-buffer
    (insert-file-contents "~/jukebox/tex/tdata.txt")
    (split-string (buffer-string) "[\\\\\n]" t)))

(defun smalldisplay-current ()
  (with-temp-buffer
    (insert-file-contents "/music/tmp/.amp.current")
    (buffer-substring (point-min) (1- (point-max)))))

(defun smalldisplay-stories ()
  (let* ((temp (smalldisplay--temp))
	 (track (smalldisplay--track))
	 (current (smalldisplay--current))
	 (image (smalldisplay '(800 . 480)
			      `((bottom-left 310 ,track)
				(top-right 0 ,temp))
			      (expand-file-name
			       "sleeve.jpg" (file-name-directory current)))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert image)
      (write-region (point-min) (point-max) "/tmp/a.png")
      (call-process "qiv" nil nil nil "/tmp/a.png"))))

(provide 'smalldisplay)

;;; smalldisplay.el ends here
