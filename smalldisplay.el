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
(require 'xcb)
(require 'eval-server)

(autoload 'jukebox-tokenize-path "jukebox")

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
  (with-current-buffer (url-retrieve-synchronously
			"http://rocket-sam/get-data.php")
    (goto-char (point-min))
    (search-forward "\n\n")
    (prog1
	(let ((data (json-read)))
	  (list (format "%.1f°C" (string-to-number
				  (cdr (assq 'temp data))))))
      (kill-buffer (current-buffer)))))

(defun smalldisplay--track ()
  (let ((track (jukebox-tokenize-path (smalldisplay--current))))
    ;; If the album and song name is the same, then drop the track
    ;; name.
    (cond
     ((or (equal (nth 1 track) (nth 2 track))
	  (and (> (length (nth 1 track)) 6)
	       (zerop (or (cl-search (nth 1 track) (nth 2 track)) -1))))
      (list (car track) (caddr track)))
     ((equal (nth 0 track) (nth 1 track))
      (list (car track) (nth 2 track)))
     (t
      track))))

(defvar smalldisplay-current-track-file "/music/tmp/.amp.current")
(defvar smalldisplay--current-track nil)

(defun smalldisplay--current ()
  (or smalldisplay--current-track
      (with-temp-buffer
	(insert-file-contents smalldisplay-current-track-file)
	(buffer-substring (point-min) (1- (point-max))))))

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
  `(cl-loop
    (condition-case err
	(progn
	  ,@body)
      (error (message "%s" err)
	     (sleep-for 10)))))

(defvar smalldisplay--notifications nil)

(defun smalldisplay-start-rocket-sam ()
  (push 'smalldisplay-display-rocket-sam smalldisplay--notifications)
  ;; Run once a minute to get temp updates.  amp updates will be
  ;; triggered via `smalldisplay-notify'.
  (run-at-time 1 60 #'smalldisplay-display-rocket-sam))

(defun smalldisplay-display-rocket-sam (&optional track)
  (setq smalldisplay--current-track track)
  (smalldisplay-stories)
  (ignore-errors
    (eval-at "lights" "dielman1" 8702 `(smalldisplay-notify)))
  (smalldisplay-quimbies)
  (smalldisplay-frame))

(defun smalldisplay-loop-quimbies ()
  (let ((last nil))
    (smalldisplay-loop
     (cl-loop
      (let ((now (file-attribute-modification-time
		  (ignore-errors
		    (file-attributes  "~/tmp/quimbies.file")))))
	(when (or (not now)
		  (not (equal last now)))
	  (setq last now)
	  (smalldisplay-display-quimbies)))
      (sleep-for 1)))))

(defun smalldisplay-mpv-id ()
  (cl-loop for (id . name) in (smalldisplay-list-windows)
	   when (and name
		     (string-match "mpv" name))
	   return id))

(defun smalldisplay-start-dielman ()
  (server-start)
  (let ((first (seq-find
		(lambda (frame)
		  (string-match "dielman1"
				(cdr (assq 'name (frame-parameters frame)))))
		(frame-list))))
    (let* ((default-minibuffer-frame first)
	   (frame
	    (make-frame '((name . "smalldisplay")
			  (minibuffer)
			  (background-color . "black")
			  (fullscreen . fullboth)))))
      (select-frame frame)))
  (fringe-mode 0)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  ;; Delay until network has stabilised.
  (run-at-time 10 nil 'smalldisplay-display-dielman))

(defvar smalldisplay--prev-dielman nil)

(defun smalldisplay-display-dielman ()
  (url-retrieve
   "http://rocket-sam/smalldisplay/image-stories-1280-800.png"
   (lambda (&rest _args)
     (goto-char (point-min))
     (search-forward "\n\n")
     (let ((image (buffer-substring (point) (point-max))))
       (kill-buffer (current-buffer))
       (unless (equal smalldisplay--prev-dielman image)
	 (setq smalldisplay--prev-dielman image)
	 (if (get-buffer "*display*")
	     (set-buffer "*display*")
	   (pop-to-buffer "*display*")
	   (delete-other-windows))
	 (erase-buffer)
	 (insert-image (create-image image 'png t
				     :scale 1))
	 (goto-char (point-min))
	 (setq mode-line-format nil
	       cursor-type nil)
	 (setq truncate-lines t)
	 (ignore-errors
	   ;; No continuation marker.
	   (set-display-table-slot standard-display-table 0 ?\ )
	   (set-display-table-slot standard-display-table 1 ?\ )))
       (redisplay t))
     (run-at-time 10 nil 'smalldisplay-display-dielman))))

(defun smalldisplay-loop-potato ()
  (let (mpv new-mpv)
    (smalldisplay-loop
     (cl-loop for i from 0
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
    (let ((track (smalldisplay--track))
	  (name "/var/www/html/smalldisplay/image-stories-1280-800.png"))
      (insert (smalldisplay '(1280 . 800)
			    `((bottom-left ,(if (= (length track) 3)
						520
					      600)
					   80
					   ,track)
			      (top-right 0 100 ,(smalldisplay--temp)))
			    (expand-file-name
			     "sleeve.jpg" (file-name-directory
					   (smalldisplay--current)))))
      (write-region (point-min) (point-max) (concat name ".tmp"))
      (rename-file (concat name ".tmp") name t))))

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
      (setq file (expand-file-name
		  "sleeve.jpg" (file-name-directory (smalldisplay--current))))
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
    (insert (smalldisplay '(1200 . 825)
			  `((bottom-right
			     600 200
			     ,(list (string-remove-suffix
				     "C"
				     (car (smalldisplay--temp))))))
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
    (write-region (point-min) (point-max)
		  "/var/www/html/frame/image-temp.rawz")
    (rename-file "/var/www/html/frame/image-temp.rawz"
		 "/var/www/html/frame/image.rawz" t)))



(defun smalldisplay-quimbies ()
  (message (format-time-string "%H:%M:%S Making"))
  (call-process "convert" nil nil nil
		"-resize" "1280x" 
		(expand-file-name
		 "sleeve.jpg" (file-name-directory
			       (smalldisplay--current)))
		"/tmp/quimbies.jpg")
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (smalldisplay '(1280 . 800)
			  `((top-left -30 260 ,(smalldisplay--track)))
			  "/tmp/quimbies.jpg"))
    (write-region (point-min) (point-max) "~/tmp/quimbies.file.tmp")
    (rename-file "~/tmp/quimbies.file.tmp" "~/tmp/quimbies.file" t)))

(defun smalldisplay-display-quimbies ()
  (call-process "xloadimage" nil nil nil
		"-display" ":1" "-onroot" "-gamma" "2"
		(expand-file-name "~/tmp/quimbies.file")))

(defvar smalldisplay-displayer nil)

(defun smalldisplay-potato (&optional debug)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (smalldisplay-potato-1
	     '(1280 . 800)
	     `((top-right 0 80 ,(list (format-time-string "%H:%M")
				      (car (smalldisplay--temp))))
	       (bottom-right 690 30 ,(smalldisplay--track)))
	     (smalldisplay-smooth
	      (cl-loop for point in (smalldisplay-rain)
		       collect (cons (* (car point) (/ 1280.0 24))
				     (- 803 (* (cdr point) 60)))))))
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
  (if (not points)
      nil
    (let ((acc 0)
	  (length 4))
      (dotimes (i length)
	(cl-incf acc (cdr (elt points i))))
      (cl-loop for i from length upto (+ 24 length)
	       collect (cons (- (car (elt points (- i (/ length 2)))) 100)
			     (prog2
				 (cl-incf acc (cdr (elt points i)))
				 (/ (* acc 1.0) (1+ length))
			       (cl-decf acc (cdr (elt points (- i length))))))))))


(defvar smalldisplay-rain nil)
(defvar smalldisplay-rain-count 0)

(defun smalldisplay-rain ()
  (if (and smalldisplay-rain
	   (not (zerop (mod (cl-incf smalldisplay-rain-count) 60))))
      ;; Serve the cached rain values usually.
      smalldisplay-rain
    (let ((rain
	   (with-current-buffer
	       (url-retrieve-synchronously
		"https://api.met.no/weatherapi/locationforecast/2.0/classic?lon=10.744587373145249&lat=59.92675174365245"
		nil nil 30)
	     (goto-char (point-min))
	     (when (search-forward "\n\n" nil t)
	       (let ((elems
		      (cl-sort
		       (seq-filter
			(lambda (e)
			  (and (dom-by-tag e 'precipitation)
			       (dom-by-tag e 'minTemperature)))
			(dom-by-tag
			 (libxml-parse-xml-region (point) (point-max))
			 'time))
		       #'string<
		       :key (lambda (e)
			      (dom-attr e 'from)))))
		 (cl-loop for i from 0
			  for elem in elems
			  for rain = (dom-by-tag elem 'precipitation)
			  collect (cons i (string-to-number
					   (dom-attr rain 'value)))))))))
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
	 (angle (+ (cl-getf line :angle)
		   (if reverse
		       float-pi
		     0)))
	 (smoothing 0.2)
	 (length (* (cl-getf line :length) smoothing)))
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
   (cl-loop for point in points
	    for i from 0
	    collect (if (zerop i)
			(format "M %s,%s" (car point) (cdr point))
		      (smalldisplay-bezier i points)))
   " "))

(defun smalldisplay-text (svg size texts &rest args)
  (cl-loop for (position y font-size strings . no-border) in texts
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
		(cl-loop for stroke in (list (max (/ font-size 16) 2) 1)
			 do (apply
			     'smalldisplay-svg-multi-line-text
			     svg strings
			     :text-anchor
			     (if (memq position '(top-right bottom-right))
				 "end"
			       "start")
			     :x (cond
				 ((memq position '(top-right bottom-right))
				  (- (car size) 20))
				 ((eq position 'top-left)
				  -60)
				 (t
				  0))
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
	(cl-loop for child in (slot-value tree 'children)
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
			    (cl-coerce
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

(defun smalldisplay-start-server ()
  (start-eval-server "lights" 8702
		     '(smalldisplay-notify)))

(defun smalldisplay-notify (&optional track)
  (dolist (func smalldisplay--notifications)
    (funcall func track)))
  
(provide 'smalldisplay)

;;; smalldisplay.el ends here
