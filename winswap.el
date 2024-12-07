;; winswap.el --- Utilities to manage windows   -*- lexical-binding:t -*-

;; Copyright (C) 2024 Evgeniy Turishev

;; Author: Evgeniy Turishev ew_turi@mail.ru

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; Simple utilites to manage windows and buffers
;; M-x winswap-shift-buffers-forward
;; M-x winswap-shift-buffers-backward


(defun winswap--layout-direction ()
  (let ((win (selected-window)))
    (cond
     ((window-combined-p win t) 'horizontal)
     ((window-combined-p win nil) 'vertical))))


(defun winswap--siblings-windows ()
  "returns list of siblings windows, returns nil if selected-window have not a parent"
  (letrec ((curr-win (selected-window))
	   (win-traverse (lambda (w)
			   (when (window-live-p w)
			     (let ((buf (window-buffer w)))
			       (cons w
				     (funcall win-traverse (window-next-sibling w))))))))
    (funcall win-traverse
	     (window-child (window-parent curr-win)))))


(defun winswap--shift-buffers (win-list)
  "shift buffers inside windows from left to right or from top to down"
  (if (length> win-list 1)
      (let* ((curr-buf (current-buffer))
	     (buffers (mapcar #'window-buffer win-list))
	     (new-buffers (cons (car (last buffers)) (butlast buffers)))
	     (win-bufs (seq-mapn #'cons win-list new-buffers)))

	(dolist (wb win-bufs)
	  (set-window-buffer (car wb) (cdr wb)))

	(let* ((new-win-buf (seq-find
			     (lambda (el)
			       (eq (cdr el) curr-buf))
			     win-bufs))
	       (new-win (car new-win-buf)))
	  (select-window new-win)))

    (message "there is only one window, ignored")))


(defun winswap-shift-buffers-forward ()
  (interactive)
  (winswap--shift-buffers (winswap--siblings-windows)))


(defun winswap-shift-buffers-backward ()
  (interactive)
  (winswap--shift-buffers (reverse (winswap--siblings-windows))))


(defun winswap-change-direction ()
  "swap layout of siblings windows, change vertical to horizontal or, conversely, horizontal to vertical,
 other windows will be closed"
  (interactive)
  (let ((win-list (winswap--siblings-windows)))
    (when (length> win-list 1)
	(let* ((is-horizontal (eq (winswap--layout-direction)
				  'horizontal))
	       (win-inx (seq-position win-list (selected-window)))
	       (win-count (length win-list))
	       (bufs (mapcar #'window-buffer win-list))
	       (split-fn (if is-horizontal #'split-window-below  #'split-window-right)))

	  (delete-other-windows)
	  
	  (let ((sz (round (/ (window-total-size nil (not is-horizontal))
		       win-count))))
	    (dotimes (_ (1- win-count))
	      (funcall split-fn (- sz)))

	    (let ((new-win-list (winswap--siblings-windows)))
	      (seq-mapn (lambda (w b) (set-window-buffer w b))
			new-win-list
			bufs)

	      (select-window (nth win-inx new-win-list))))))))


(provide 'winswap)
;;; winswap.el ends here
