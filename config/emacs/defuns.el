
;;; defuns.el --- Personal function definitions

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defmacro user:filter (fn list)
  `(delq nil (mapcar (lambda (l) (and (funcall ,fn l) l)) ,list)))

(defun user:match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

(defun user:indent-whole-buffer()
  "Indent Whole Buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun user:kill-current-buffer ()
  "Kill the current buffer without prompting the user"
  (interactive)
  (kill-buffer (current-buffer)))

(defun user:daily-notes ()
  (interactive)
  (find-file (format-time-string "~/.emacs.d/org/daily-notes/%Y-%m-%d.org")))

;; end of defuns.el
