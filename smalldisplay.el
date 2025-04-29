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

(require 'svg)
(require 'eval-server)

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
	;; Scale image down a bit because imagemagick doesn't
	;; like big embeds.
	(when (> (car image-size) 2000)
	  (call-process-region (point-min) (point-max)
			       "convert" t t nil
			       "-resize" "2000x"
			       image "/tmp/sleeve.jpg")
	  (setq image-size (smalldisplay-image-size image)
		image "/tmp/sleeve.jpg"))
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
      (write-region (point-min) (point-max) "/tmp/a.svg")
      (call-process-region (point-min) (point-max) "rsvg-convert"
			   t (current-buffer))
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
				 (svg--encode-text text)))))))

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

(defun smalldisplay-path-element (n path)
  "Return the Nth reversed element in PATH."
  (while (not (zerop n))
    (setq path (directory-file-name (file-name-directory path)))
    (cl-decf n))
  (file-name-nondirectory path))

(defun smalldisplay-tokenize-path (file)
  (let ((track (smalldisplay-path-element 0 file))
	(album (smalldisplay-path-element 1 file))
	(group (smalldisplay-path-element 2 file)))
    (setq album (replace-regexp-in-string " +([0-9]+)\\'" "" album))
    (setq album (replace-regexp-in-string " +([0-9]+):" ":" album))
    (setq track (replace-regexp-in-string
		 "\\`[0-9][0-9]-\\|[.]\\(flac\\|mp3\\)\\'" "" track))
    (when (string-match " - " track)
      (let ((split (split-string track " - ")))
	(setq group (car split)
	      track (cadr split))))
    (list group album track)))

(defun smalldisplay--track ()
  (let ((track (smalldisplay-tokenize-path (smalldisplay--current))))
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

(defmacro smalldisplay-loop (&rest body)
  `(cl-loop
    (condition-case err
	(progn
	  ,@body)
      (error (message "%s" err)
	     (sleep-for 10)))))

(defvar smalldisplay--notifications nil)

(defun smalldisplay-start-rocket-sam ()
  (smalldisplay-start-server)
  (push 'smalldisplay-display-rocket-sam smalldisplay--notifications)
  ;; Run once a minute to get temp updates.  amp updates will be
  ;; triggered via `smalldisplay-notify'.
  (run-at-time 1 60 #'smalldisplay-perhaps-display-rocket-sam)
  (run-at-time 600 600 #'smalldisplay-frame))

(defvar smalldisplay--last-update nil)
(defun smalldisplay-perhaps-display-rocket-sam ()
  (when (or (not smalldisplay--last-update)
	    (> (- (float-time) smalldisplay--last-update)
	       600))
    (setq smalldisplay--last-update (float-time))
    (smalldisplay-display-rocket-sam)))

(defun smalldisplay-display-rocket-sam (&optional track)
  (when track
    (setq smalldisplay--current-track track))
  (smalldisplay-make-dielman1-image)
  (smalldisplay-make-dielman4-image)
  (ignore-errors
    (eval-at-async "lights" "dielman1" 8703 `(smalldisplay-notify)))
  (ignore-errors
    (eval-at-async "lights" "dielman4" 8703 `(smalldisplay-notify)))
  (ignore-errors
    (eval-at-async "lights" "tube" 8703 `(smalldisplay-notify ,track))))

(defun smalldisplay-mpv-id ()
  (cl-loop for pid in (list-system-processes)
	   for atts = (process-attributes pid)
	   for name = (cdr (assq 'args atts))
	   when (and name
		     (string-match "/mpv" name))
	   return pid))

(defun smalldisplay-start-dielman ()
  (smalldisplay-start-server)
  (push 'smalldisplay-display-dielman smalldisplay--notifications)
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

(defvar smalldisplay--dielman-buffer nil)

(defun smalldisplay-display-dielman (&optional _track)
  (when smalldisplay--dielman-buffer
    ;; This will kill any transfers that are in progress.
    (when-let ((proc (get-buffer-process smalldisplay--dielman-buffer)))
      (set-process-filter proc nil)
      (set-process-sentinel proc nil)
      (delete-process proc))
    (kill-buffer smalldisplay--dielman-buffer))
  (setq smalldisplay--dielman-buffer (smalldisplay-display-dielman-1)))

(defun smalldisplay-display-dielman-1 ()
  (url-retrieve
   (format "http://rocket-sam/smalldisplay/%s"
	   (if (equal (system-name) "dielman1")
	       "image-dielman1-1280-800.png"
	     "image-dielman4-1280-800.png"))
   (lambda (&rest _args)
     (goto-char (point-min))
     (search-forward "\n\n")
     (let ((image (buffer-substring (point) (point-max))))
       (when-let ((proc (get-buffer-process (current-buffer))))
	 (delete-process proc))
       (kill-buffer (current-buffer))
       (let ((frame (seq-find
		     (lambda (frame)
		       (string-match
			"smalldisplay"
			(cdr (assq 'name (frame-parameters frame)))))
		     (frame-list))))
	 (select-frame frame))
       (if (get-buffer "*display*")
	   (set-buffer "*display*")
	 (pop-to-buffer "*display*")
	 (delete-other-windows))
       (buffer-disable-undo)
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
     (redisplay t))))

(defun smalldisplay-start-tube ()
  ;; Have a black background to avoid glitches when mpv restarts.
  (start-process "blackness" nil "~/src/pqiv/pqiv"
		 "-f" "-i"
		 (expand-file-name "~/src/smalldisplay.el/black.png"))
  (setq smalldisplay-current-track-file "/tmp/.amp.current")
  (smalldisplay-start-server)
  (setq smalldisplay--current-track (smalldisplay--current))
  (push 'smalldisplay-display-tube smalldisplay--notifications)
  (smalldisplay-loop-tube))

(defun smalldisplay-display-tube (&optional track)
  (message "Track: %s" track)
  (when track
    (setq smalldisplay--current-track track)
    (with-temp-buffer
      (insert track "\n")
      (write-region (point-min) (point-max) smalldisplay-current-track-file
		    nil 'silent))))

(defun smalldisplay-loop-tube ()
  (let ((track smalldisplay--current-track)
	mpv new-mpv)
    (smalldisplay-loop
     (cl-loop for i from 0
	      when (or
		    (not (equal track smalldisplay--current-track))
		    (not (equal (setq new-mpv (smalldisplay-mpv-id)) mpv))
		    (zerop (mod i 30)))
	      do (smalldisplay-tube)
	      (setq mpv new-mpv
		    track smalldisplay--current-track)
	      do (sleep-for 1)
	      (message smalldisplay--current-track)))))

(defun smalldisplay-make-dielman1-image ()
  (message (format-time-string "%H:%M:%S Making"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((track (smalldisplay--track))
	  (name "/var/www/html/smalldisplay/image-dielman1-1280-800.png"))
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

(defun smalldisplay-make-dielman4-image ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((track (smalldisplay--track))
	  (name "/var/www/html/smalldisplay/image-dielman4-1280-800.png"))
      (insert (smalldisplay '(1280 . 800)
			    `((top-left -30 260 ,(smalldisplay--track)))
			    (expand-file-name
			     "sleeve.jpg" (file-name-directory
					   (smalldisplay--current)))))
      (write-region (point-min) (point-max) (concat name ".tmp"))
      (rename-file (concat name ".tmp") name t))))

(require 'seq)

(defun smalldisplay-frame ()
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
    (set-process-sentinel
     ;; This is very slow, so do it in the background.
     (start-process "sharp" (get-buffer-create "*sharp*")
		    "sharp"
		    "-m" "1" "-f" "10"
		    "/tmp/trim.jpg" "/tmp/sharp.jpg")
     (lambda (proc _change)
       (unless (process-live-p proc)
	 (with-temp-buffer
	   (set-buffer-multibyte nil)
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
					    ;; Use a smaller minus.
					    (string-replace
					     "-" "‐"
					     (car (smalldisplay--temp)))))))
				 "/tmp/sleeve-stretch.jpg"))
	   (write-region (point-min) (point-max) "/tmp/a.png")
	   (thread-yield)
	   (call-process-region (point-min) (point-max)
				"convert"
				t (current-buffer) nil
				"png:-"
				;;"-rotate" "180"
				"-depth" "4"
				"pgm:-")
	   (thread-yield)
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
			"/var/www/html/frame/image.rawz" t)))))))

(defvar smalldisplay-displayer nil)

(defun smalldisplay-tube (&optional debug)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (smalldisplay-tube-1
	     '(1280 . 800)
	     `((top-right 0 80 ,(list (format-time-string "%H:%M")
				      (car (smalldisplay--temp))))
	       (bottom-right 690 30 ,(smalldisplay--track)))
	     (smalldisplay-smooth
	      (cl-loop for point in (smalldisplay-rain)
		       collect (cons (* (car point) (/ 1280.0 24))
				     (- 803 (* (cdr point) 60)))))))
    (write-region (point-min) (point-max) "/tmp/a.png" nil 'silent)
    (if debug
	(call-process-region (point-min) (point-max)
			     "feh" nil nil nil "-ZF" "/tmp/a.png")
      (let ((prev smalldisplay-displayer))
	(setq smalldisplay-displayer
	      (start-process "qiv" nil "~/src/pqiv/pqiv"
			     "-c" "-f" "-i" "/tmp/a.png"))
	(when prev
	  (sleep-for 0.1)
	  (delete-process prev))))))

(defun smalldisplay-tube-1 (size texts rain)
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
	(write-region (point-min) (point-max) "/tmp/b.svg" nil 'silent)
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
		   (if (memq position '(top-right bottom-right
						  top-right-rotated))
		       "end"
		     "start")
		   :x (if (memq position '(top-right bottom-right
						     top-right-rotated))
			  (- (car size) 20)
			20)
		   :y y
		   :font-size font-size
		   :font-weight "bold"
		   :fill "white"
		   :font-family "futura"
		   (if (eq position 'top-right-rotated)
		       `(:transform: "translateX(-50%) translateY(-50%)"
				     ,@args)
		     args))
		(cl-loop for stroke in (list (max (/ font-size 16) 2) 1)
			 do (apply
			     'smalldisplay-svg-multi-line-text
			     svg strings
			     :text-anchor
			     (if (memq position '(top-right bottom-right))
				 "end"
			       "start")
			     :x (cond
				 ((memq position '(top-right
						   bottom-right
						   top-right-rotated))
				  (- (car size) 20))
				 ((eq position 'top-left)
				  20)
				 (t
				  0))
			     :y (or y
				    (if (memq position '(bottom-left
							 bottom-right))
					(- (cdr size) (* (length texts) 100)
					   20)
				      20))
			     :font-size font-size
			     :stroke "black"
			     :stroke-width (format "%dpx" stroke)
			     :font-weight "bold"
			     :fill "white"
			     :font-family "futura"
			     (if (eq position 'top-right-rotated)
				 `(:transform
				   ,(format "translate(%s,-%s) rotate(90)"
					    (- (car size) 20)
					    (- (cdr size) 40))
				   ,@args)
			       args))))))

(defun smalldisplay-start-server ()
  (start-eval-server "lights" 8703
		     '(smalldisplay-notify)))

(defun smalldisplay-notify (&optional track)
  ;; Return immediately and then run notification.
  (run-at-time 0.1 nil 'smalldisplay--run-notifications track)
  nil)

(defun smalldisplay--run-notifications (track)
  (dolist (func smalldisplay--notifications)
    (funcall func track)))

(defun smalldisplay--next-clock ()
  (+ (- 60 (% (time-convert (current-time) 'integer) 60)) 30))

(defun smalldisplay-clock-runner ()
  (run-at-time (smalldisplay--next-clock) nil
	       (lambda ()
		 (smalldisplay-clock)
		 (smalldisplay-clock-runner))))

(defun smalldisplay-clock (&optional testing)
  (let* ((dia 720)
	 (rad (/ dia 2))
	 (svg (svg-create dia dia))
	 ;; The next minute -- we're generating in advance.
	 (time (decode-time (+ (time-convert (current-time) 'integer) 60)))
	 (back "black")
	 (fore "white"))
    (svg-gradient svg "gradient" 'nope '((0 . "#000080") (100 . "black")))
    (svg-rectangle svg 0 0 dia dia :fill "black")
    (svg-circle svg rad rad rad  :gradient "gradient")
    (dotimes (i 60)
      (svg-line svg 0 0 0 rad
		:stroke-width "5px"
		:stroke "grey"
		:transform (format "translate(%d,%d) rotate(%d)"
				   rad rad
				   (* i (/ 360 60)))))
    (svg-circle svg rad rad (- rad 10) :gradient "gradient")
    (dotimes (i 60)
      (when (zerop (% i 5))
	(svg-line svg 0 0 0 rad
		  :stroke-width "10px"
		  :stroke fore
		  :transform (format "translate(%d,%d) rotate(%d)"
				     rad rad
				     (* i (/ 360 60))))))
    (svg-circle svg rad rad (- rad 20) :gradient "gradient")
    (svg-line svg 0 0 0 (+ (- rad) 60)
	      :stroke-width "20px"
	      :stroke fore
	      :stroke-linecap "round"
	      :transform (format "translate(%d,%d) rotate(%d)"
				 rad rad
				 (* (decoded-time-minute time)
				    (/ 360 60))))
    (svg-line svg 0 0 0 (+ (- rad) 150)
	      :stroke-width "20px"
	      :stroke fore
	      :stroke-linecap "round"
	      :transform (format "translate(%d,%d) rotate(%d)"
				 rad rad
				 (* (+ (decoded-time-hour time)
				       (/ (decoded-time-minute time) 60.0))
				    (/ 360 12))))
    (when nil
      (svg-rectangle svg (- dia 160) (- rad 40) 110 80
		     :stroke-width "0px"
		     :stroke fore
		     :fill "grey"))
    (svg-text svg (format "%s %d"
			  (elt
			   '("dim" "lun" "mar" "mer" "jeu" "ven" "sam")
			   (decoded-time-weekday time))
			  (decoded-time-day time))
	      :x (- dia 50)
	      :y (+ rad 20)
	      :font-size 60
	      :stroke "black"
	      :stroke-width "0px"
	      :font-weight "bold"
	      :text-anchor "end"
	      :fill "grey"
	      :font-family "futura")
    (svg-text svg (replace-regexp-in-string "C$" "" (car (smalldisplay--temp)))
	      :x 50
	      :y (+ rad 20)
	      :font-size 60
	      :stroke "grey"
	      :stroke-width "0px"
	      :font-weight "bold"
	      :fill "grey"
	      :font-family "futura")
    (with-temp-buffer
      (svg-print svg)
      (call-process-region (point-min) (point-max) "rsvg-convert"
			   t (current-buffer))
      (write-region (point-min) (point-max) "/stage/tmp/clock.png"
		    nil 'silent)
      (call-process
       "scp" nil nil nil
       "-i" (expand-file-name "~/src/smalldisplay.el/round_key")
       "/stage/tmp/clock.png" "192.168.1.242:/mnt/tmpfs/"))
    (when testing
      (find-file "/stage/tmp/clock.png"))))

(provide 'smalldisplay)

;;; smalldisplay.el ends here
