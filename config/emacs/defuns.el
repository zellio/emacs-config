;;; config/emacs/defuns.el --- personal function definitions

;; Copyright (C) 2012-2016 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defmacro user/filter (fn list)
  `(delq nil (mapcar (lambda (l) (and (funcall ,fn l) l)) ,list)))

(defun user/match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

(defun user/indent-whole-buffer ()
  "Indent Whole Buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))
  ;; (untabify (point-min) (point-max)))

(defun user/kill-current-buffer ()
  "Kill the current buffer without prompting the user"
  (interactive)
  (kill-buffer (current-buffer)))

(defun user/revert-all-buffers ()
  "Replace all buffers with their visited file contents on disk."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and
             (buffer-file-name)
             (file-exists-p (buffer-file-name))
             (buffer-modified-p))
        (revert-buffer t t t)
        (message "Reverted buffer %s" buffer)))))

(defun user/serialize (data path)
  "Serialize the s-expr DATA into file at PATH."
  (let ((dir (file-name-directory path)))
    (if (not (file-directory-p dir))
        (mkdir dir t))
    (with-temp-buffer
      (insert
       ";; -*- mode: lisp; coding: utf-8 -*-\n"
       ";; emacs-version: " emacs-version "\n"
       "\n"
       (prin1-to-string data))
      (write-file path nil))
    ))

(defun user/deserialize (file)
  "Deserialize the file at PATH into an s-expr."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer)))
    '()))

;;; config/emacs/defuns.el ends here
