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
(require 'svg-aux)
(require 'icalendar)

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
  (run-at-time 600 600 #'smalldisplay-frame)
  (smalldisplay-clock-runner))

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
  (smalldisplay-make-clock-image)
  ;;(smalldisplay-seeedframe)
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
    (let ((name "/var/www/html/smalldisplay/image-dielman4-1280-800.png"))
      (insert (smalldisplay '(1280 . 800)
			    `((top-left -30 260 ,(smalldisplay--track)))
			    (expand-file-name
			     "sleeve.jpg" (file-name-directory
					   (smalldisplay--current)))))
      (write-region (point-min) (point-max) (concat name ".tmp"))
      (rename-file (concat name ".tmp") name t))))

(defun smalldisplay-make-clock-image ()
  (call-process "convert" nil nil nil
		(expand-file-name
		 "sleeve.jpg" (file-name-directory
			       (smalldisplay--current)))
		"-gravity" "center" "-crop" "1:1" "+repage"
		"-resize" "720x720"
		"/var/www/html/smalldisplay/sleeve720.png"))

(require 'seq)

(defun smalldisplay-frame ()
  (let* ((file (expand-file-name
		"sleeve.jpg" (file-name-directory (smalldisplay--current)))))
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
    (svg-smooth-line
     svg rain
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
      (cl-loop for i from length upto (min (1- (length points)) (+ 24 length))
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

(defun smalldisplay-text (svg size texts &rest args)
  (cl-loop with filter = (svg-outline svg 3 "black" 1)
	   for (position y font-size strings . no-border) in texts
	   do (apply
	       'svg-multi-line-text
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
	       :stroke-width 0
	       :font-weight "bold"
	       :fill "white"
	       :font-family "futura"
	       :filter (if no-border "" filter)
	       (if (eq position 'top-right-rotated)
		   `(:transform
		     ,(format "translate(%s,-%s) rotate(90)"
			      (- (car size) 20)
			      (- (cdr size) 40))
		     ,@args)
		 args))))

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

(defvar smalldisplay--scp-process nil)

(defun smalldisplay-clock (&optional testing)
  (let* ((dia 720)
	 (rad (/ dia 2))
	 (svg (svg-create dia dia))
	 ;; The next minute -- we're generating in advance.
	 (time (decode-time (+ (time-convert (current-time) 'integer) 60)))
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
      (when smalldisplay--scp-process
	(delete-process smalldisplay--scp-process))
      (setq smalldisplay--scp-process
	    (start-process
	     "scp" nil
	     "scp" "-i" (expand-file-name "~/src/smalldisplay.el/round_key")
	     "/stage/tmp/clock.png" "192.168.1.242:/mnt/tmpfs/")))
    (when testing
      (find-file "/stage/tmp/clock.png"))))

(defun smalldisplay-seeedframe ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (smalldisplay '(1200 . 1600)
			  `((top-right
			     20 200
			     ,(list (string-remove-suffix
				     "C"
				     ;; Use a smaller minus.
				     (string-replace
				      "-" "‐"
				      (car (smalldisplay--temp)))))))
			  (expand-file-name
			   "sleeve.jpg" (file-name-directory
					 (smalldisplay--current)))))
    (write-region
     (point-min) (point-max)
     "/var/www/html/smalldisplay/image-seeedframe-pre.png")
    (call-process "~/src/seeedframe/photoframe-upload.sh" nil nil nil
		  "/var/www/html/smalldisplay/image-seeedframe-pre.png"
		  "--file"
		  "/var/www/html/smalldisplay/image-seeedframe.png")))

(defun smalldisplay-make-calstation ()
  (with-temp-buffer
    ;;(set-buffer-multibyte nil)
    (svg-print (smalldisplay-calstation))
    (write-region (point-min) (point-max) "/tmp/a.svg")
    (call-process-region (point-min) (point-max) "rsvg-convert"
			 t (current-buffer))
    (write-region
     (point-min) (point-max)
     "/var/www/html/smalldisplay/image-seeedframe-pre.png")
    (call-process "~/src/seeedframe/photoframe-upload.sh" nil nil nil
		  "/var/www/html/smalldisplay/image-seeedframe-pre.png"
		  "--file"
		  "/var/www/html/smalldisplay/image-seeedframe.png")))

(defun smalldisplay-calstation ()
  (interactive)
  (unless smalldisplay-calendar-entries
    (smalldisplay-get-calendar))
  (let* ((width 1200)
	 (height 1600)
	 (margin 2)
	 (svg (svg-create width height)))
    (svg-rectangle svg 0 0 width height
		   :fill "white")
    ;; Find the start date.  We want a Monday two weeks before the
    ;; current month.
    (let* ((now (time-convert (current-time) 'integer))
	   (time (decode-time now))
	   (ctop (+ 950 margin))
	   (hstride 64)
	   (wstride (/ (- width (* margin 2)) 7)))
      (svg-rectangle svg margin ctop (- width (* margin 2)) (* hstride 8)
		     :fill "#ffff87")
      (setf (decoded-time-day time) 1)
      (setq time (decoded-time-add time (make-decoded-time :day -7)))
      (cl-loop while (> (decoded-time-weekday (decode-time (encode-time time)))
			1)
	       do (setq time (decoded-time-add
			      time (make-decoded-time :day -1))))
      (cl-loop for week from 0 upto 8
	       do
	       (svg-line svg margin (+ ctop (* week hstride))
			 (+ margin (* wstride 7)) (+ ctop (* week hstride))
			 :stroke-width 2
			 :stroke-color "black"))
      (cl-loop for day from 0 upto 7
	       do
	       (svg-line svg
			 (+ margin (* day wstride)) ctop
			 (+ margin (* day wstride)) (+ ctop (* 8 hstride))
			 :stroke-width 2
			 :stroke-color "black"))
      (cl-loop for day from 0 upto (1- (* 7 8))
	       do
	       (when (member (mod day 7) '(5 6))
		 (svg-rectangle svg
				(+ 1 margin (* (mod day 7) wstride))
				(+ 1 ctop (* (/ day 7) hstride))
				(- wstride 2)
				(- hstride 2)
				:fill-color "#b7c07a"))
	       (when (and (= (decoded-time-month time)
			     (decoded-time-month (decode-time now)))
			  (= (decoded-time-day time)
			     (decoded-time-day (decode-time now))))
		 (svg-rectangle svg
				(+ 1 margin (* (mod day 7) wstride))
				(+ 1 ctop (* (/ day 7) hstride))
				(- wstride 2)
				(- hstride 2)
				:fill-color "#d36863"))
	       (svg-text svg (format "%d" (decoded-time-day time))
			 :x (+ margin (- wstride 8) (* (mod day 7) wstride))
			 :y (+ 30 ctop (* (/ day 7) hstride))
			 :font-size 25
			 :text-anchor "end"
			 :font-weight "normal"
			 :fill (if (= (decoded-time-month time)
				      (decoded-time-month (decode-time now)))
				   "black"
				 "grey")
			 :font-family "Coconino County")
	       (cl-loop for event in (smalldisplay-calendar-entries time)
			for i from 0
			for text =
			(string-limit
			 (string-trim
			  (or (nth 2 (assq 'SUMMARY event)) "*"))
			 20)
			do
			(svg-text
			 svg text
			 :x (+ margin 10 (* (mod day 7) wstride))
			 :y (+ 20 ctop (* (/ day 7) hstride) (* i 15))
			 :font-size 15
			 :text-anchor "start"
			 :font-weight "normal"
			 :fill (if (= (decoded-time-month time)
				      (decoded-time-month (decode-time now)))
				   "black"
				 "grey")
			 :font-family "Coconino County"))
	       (setq time (decoded-time-add time (make-decoded-time :day 1))))
      (svg-embed svg
		 (expand-file-name "~/src/smalldisplay.el/circle1.png")
		 "image/png"
		 nil
		 :width wstride
		 :height (- hstride 1)
		 :preserveAspectRatio "none meet"
		 :x (+ margin (* 6 wstride))
		 :y (+ ctop (* hstride 7) 0.5))      
      (svg-embed svg
		 (seq-random-elt
		  (directory-files "~/src/smalldisplay.el/krazy/strips/"
				   t "\\.png\\'"))
		 "image/png"
		 nil
		 :width (- width (* margin 2) -5)
		 :preserveAspectRatio "xMinYMin meet"
		 :x (- margin 2)
		 :y (+ ctop (* hstride 8) 6))

      ;; Heading.
      (cl-loop with wstart = margin
	       with hstart = 200
	       with hwidth = (- width 210)
	       for (size color) in '((15 "black")
				     (20 "#ffff87")
				     (2 "black")
				     (2 "white")
				     (2 "black"))
	       do
	       (svg-rectangle svg wstart wstart
			      (- hwidth (* wstart 2))
			      hstart
			      :fill color)
	       (cl-incf wstart size)
	       (cl-decf hstart (* size 2))
	       finally
	       (svg-rectangle svg wstart wstart
			      (- hwidth (* wstart 2))
			      hstart
			      :fill "#d36863")
	       (svg-text
		svg
		(let ((dnow (decode-time now)))
		  (format "%s %d %s %d"
			  (elt '("lundi" "mardi" "mercredi" "jeudi"
				 "vendredi" "samedi" "dimanche")
			       (1- (decoded-time-weekday dnow)))
			  (decoded-time-day dnow)
			  (elt '("" "janvier" "fevrier" "mars" "avril" "mai"
				 "june" "juillet" "ôut" "september" "octobre"
				 "novembre" "decembre")
			       (decoded-time-month dnow))
			  (decoded-time-year dnow)))
		:x (/ hwidth 2)
		:y (+ wstart 80)
		:font-size 50
		:text-anchor "middle"
		:font-weight "normal"
		:fill "black"
		:font-family "Coconino County Smooth"))
      ;; Temperature summary.
      (svg-rectangle svg (- width 200 margin) margin
		     200 200
		     :fill "black")
      (svg-rectangle svg (- width 200 -2 margin) (+ margin 2)
		     196 196
		     :fill "#7888c8")
      (let ((summary (smalldisplay-weather-summary
		      (smalldisplay-weather-data (smalldisplay-date)))))
	(svg-embed svg (expand-file-name "~/src/smalldisplay.el/roundthing1.png")
		   "image/png" nil
		   :x (- width margin 140)
		   :y 60
		   :preserveAspectRatio "xMinYMin meet"
		   :height "75px")
	(svg-text
	 svg (format "%d°-%d°"
		     (plist-get summary :min-temp)
		     (plist-get summary :max-temp))
	 :x (- width margin (/ 200 2))
	 :y (+ margin 50)
	 :font-size 40
	 :text-anchor "middle"
	 :font-weight "normal"
	 :fill "black"
	 :font-family "Coconino County Smooth")
	(svg-text
	 svg (format "%dmm" (plist-get summary :rain))
	 :x (- width margin (/ 200 2))
	 :y (+ margin 180)
	 :font-size 40
	 :text-anchor "middle"
	 :font-weight "normal"
	 :fill "black"
	 :font-family "Coconino County Smooth"))

      ;; Weather.
      (let ((wheight 503)
	    (wtop (+ 386 margin))
	    (weather (smalldisplay-weather-data (smalldisplay-date))))
	(svg-rectangle svg margin 210
		       (- width (* margin 2)) 145
		       :fill "black")
	(svg-embed svg (expand-file-name "~/src/smalldisplay.el/pattern3.png")
		   "image/png" nil
		   :x margin
		   :y (+ 230 margin)
		   :preserveAspectRatio "xMinYMin meet"
		   :width (- width (* margin 2)))
	;; Weather box.
	(svg-embed svg (expand-file-name "~/src/smalldisplay.el/frame1.jpg")
		   "image/jpeg" nil
		   :x margin
		   :y (+ 360 margin)
		   :preserveAspectRatio "xMinYMin meet"
		   :width (- width (* margin 2)))
	(svg-rectangle svg
		       (+ margin 15) wtop
		       (- width (* margin 2) 30) wheight
		       :stroke-width "2px"
		       :stroke-color "black"
		       :fill "#7888c8")
	(when t
	  (svg-rectangle svg
			 (+ margin 15 1)  (+ wtop 330)
			 (- width (* margin 2) 30 2) (- wheight 330)
			 :fill "#1a0107"))
	(let ((rain (smalldisplay-weather-rain weather)))
	  (svg-smooth-line
	   svg
	   (smalldisplay-smooth
	    (cl-loop for pval in rain
		     for i from 0
		     collect (cons (+ (* i (/ (- width (* margin 2) -25)
					      (- (float (length rain)) 4)))
				      margin -10)
				   (+ wtop
				      (- wheight (* (/ pval 24.0) wheight))))))
	   :stroke-width 7
	   :fill "none"
	   :stroke "white"))
	;; Sunmoon clouds.
	(cl-loop for x from 0 upto 24 by 3
		 with stride = (/ (- width (* margin 2)) 24.0)
		 do
		 (unless (memq x '(0 24))
		   (svg-text
		    svg (format "%02d" x)
		    :x (+ margin (* stride x))
		    :y (+ wtop wheight -10)
		    :font-size 20
		    :text-anchor "middle"
		    :font-weight "normal"
		    :fill "white"
		    :font-family "Coconino County"))
		 (when-let ((elem (smalldisplay-weather-hour weather
							     (1+ x))))
		   (let* ((cloud (string-to-number
				  (dom-attr (dom-by-tag elem 'cloudiness)
					    'percent)))
			  (elevation 
			   (smalldisplay-oslo-solar-elevation
			    (format "%s %02d:30" (smalldisplay-date)
				    (1+ x))))
			  (sunmoon (if (< elevation 0)
				       "~/src/smalldisplay.el/moon5.png"
				     "~/src/smalldisplay.el/sun2.png"))
			  (ypos (- wtop (* elevation 6) -90))
			  (cwidth (* cloud 2.6)))
		     (svg-embed
		      svg
		      (expand-file-name sunmoon)
		      "image/png" nil
		      :x (+ margin (* stride x) (/ (* stride 3) 2)
			    (if (< elevation 0)
				-35
			      -50))
		      :y (+ ypos 200
			    (if (< elevation 0)
				10
			      0))
		      :preserveAspectRatio "xMinYMin meet"
		      :width (if (< elevation 0)
				 70
			       100))
		     (when (> cloud 0)
		       (svg-embed
			svg
			(expand-file-name "~/src/smalldisplay.el/clouds1.png")
			"image/png" nil
			:x (+ margin (* stride x) (/ (* stride 3) 2)
			      (- (/ cwidth 2)))
			:y (+ ypos 180)
			:width cwidth))
		     (when (> cloud 80)
		       (svg-embed
			svg
			(expand-file-name "~/src/smalldisplay.el/lightning2.png")
			"image/png" nil
			:preserveAspectRatio "xMinYMin meet"
			:x (+ margin (* stride x) (/ (* stride 3) 2)
			      -25)
			:y (+ ypos 260)
			:width 50)))))
	(when nil
	  (svg-embed svg (expand-file-name "~/src/smalldisplay.el/pattern1.png")
		     "image/png" nil
		     :x margin
		     :y (+ wtop wheight)
		     :width (- width (* margin 2)))))
      )
    (with-current-buffer (get-buffer-create "*calstation*")
      (erase-buffer)
      (insert-image (svg-image svg :max-width 1100))
      (insert "\n\n")
      (when-let ((window (get-buffer-window nil t)))
	(set-window-point window (point-max))))
    svg))

(defun smalldisplay-weather-hour (weather hour)
  (cl-loop for elem in weather
	   when (and
		 (dom-by-tag elem 'cloudiness)
		 (equal
		  (format "%sT%02d:" (smalldisplay-date) hour)
		  (substring (dom-attr elem 'from) 0 14)))
	   return elem))

(defun smalldisplay-date ()
  (format-time-string "%F")
  ;;"2026-08-29"
  )

(defun smalldisplay-calendar-entries (time)
  (cl-loop for event in (nth 3 (car smalldisplay-calendar-entries))
	   for start = (iso8601-parse (caddr (assq 'DTSTART (nth 2 event))))
	   when (and (= (decoded-time-year time) (decoded-time-year start))
		     (= (decoded-time-month time) (decoded-time-month start))
		     (= (decoded-time-day time) (decoded-time-day start)))
	   collect (nth 2 event)))

(defvar smalldisplay-calendar-url nil)
(defvar smalldisplay-calendar-entries nil)

(defun smalldisplay-get-calendar ()
  (setq
   smalldisplay-calendar-entries
   (with-current-buffer (url-retrieve-synchronously smalldisplay-calendar-url)
     (goto-char (point-min))
     (prog1
	 (when (search-forward "\n\n" nil t)
	   (decode-coding-region (point) (point-max) 'utf-8)
	   (icalendar--read-element nil nil))
       (kill-buffer (current-buffer))))))

(defvar smalldisplay-weather-data nil)

(defun smalldisplay-get-weather-data ()
  (with-current-buffer
      (url-retrieve-synchronously
       "https://api.met.no/weatherapi/locationforecast/2.0/classic?lon=10.744587373145249&lat=59.92675174365245"
       nil nil 30)
    (goto-char (point-min))
    (prog1
	(and (search-forward "\n\n" nil t)
	     (libxml-parse-xml-region (point) (point-max)))
      (kill-buffer (current-buffer)))))

(defun smalldisplay-weather-data (date)
  (cl-loop for point in (dom-by-tag (or smalldisplay-weather-data
					(setq smalldisplay-weather-data
					      (smalldisplay-get-weather-data)))
				    'time)
	   when (equal (substring (dom-attr point 'from) 0 10) date)
	   collect point))

(defun smalldisplay-weather-rain (points)
  (cl-loop for elem in points
	   when (and (dom-by-tag elem 'precipitation)
		     (not (dom-by-tag elem 'minTemperature)))
	   collect (+
		    (random 24)
		    (string-to-number
		     (dom-attr (dom-by-tag elem 'precipitation) 'value)))))

(defun smalldisplay-weather-summary (weather)
  (cl-loop for elem in weather
	   for temp = (dom-attr (dom-by-tag elem 'temperature) 'value)
	   for rain = (dom-attr (dom-by-tag elem 'precipitation) 'value)
	   when temp
	   maximize (string-to-number temp) into max-temp
	   when temp
	   minimize (string-to-number temp) into min-temp
	   when rain
	   sum (string-to-number rain) into rain-total
	   finally (return (list :max-temp max-temp
				 :min-temp min-temp
				 :rain rain-total))))

(defun smalldisplay-elevation (lat lon &optional time)
  "Return the sun's elevation in degrees at LAT/LON at TIME.
LAT and LON are in degrees, LON positive east.  TIME is an Emacs
time value (defaults to now).  Negative results mean the sun is
below the horizon.  Atmospheric refraction is not included."
  (let* ((time (or time (current-time)))
         (deg (/ float-pi 180.0))
         (dt (decode-time time t))      ; decode in UTC
         (doy (string-to-number (format-time-string "%j" time t)))
         (hour (+ (decoded-time-hour dt)
                  (/ (decoded-time-minute dt) 60.0)
                  (/ (decoded-time-second dt) 3600.0)))
         ;; Fractional year, in radians.
         (gamma (* (/ (* 2 float-pi) 365.0)
                   (+ (1- doy) (/ (- hour 12) 24.0))))
         ;; Equation of time, in minutes.
         (eqtime (* 229.18
                    (+ 0.000075
                       (*  0.001868 (cos gamma))
                       (* -0.032077 (sin gamma))
                       (* -0.014615 (cos (* 2 gamma)))
                       (* -0.040849 (sin (* 2 gamma))))))
         ;; Solar declination, in radians.
         (decl (+ 0.006918
                  (* -0.399912 (cos gamma))
                  (*  0.070257 (sin gamma))
                  (* -0.006758 (cos (* 2 gamma)))
                  (*  0.000907 (sin (* 2 gamma)))
                  (* -0.002697 (cos (* 3 gamma)))
                  (*  0.001480 (sin (* 3 gamma)))))
         ;; True solar time in minutes (hour is UTC, so no tz term).
         (tst (+ (* hour 60.0) eqtime (* 4.0 lon)))
         ;; Hour angle, in radians.
         (ha (* deg (- (/ tst 4.0) 180.0)))
         (lat-r (* deg lat))
         (sin-h (+ (* (sin lat-r) (sin decl))
                   (* (cos lat-r) (cos decl) (cos ha)))))
    (/ (asin sin-h) deg)))

(defun smalldisplay-oslo-solar-elevation (&optional time-string)
  (let* ((time (if (or (null time-string)
                       (string-empty-p time-string))
                   (current-time)
                 (date-to-time time-string)))
         (h (smalldisplay-elevation 59.91 10.75 time)))
    h))

(provide 'smalldisplay)

;;; smalldisplay.el ends here
