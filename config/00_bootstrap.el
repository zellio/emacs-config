;;; config/emacs/00_bootstrap.el --- set up initial config environment

;; Copyright (C) 2012-2021 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prep emacs for further configuration

;;; Code:


;;; Set up package management

(setq
 ;;; Fix bug
 gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"

 ;; This is an ugly hack becuase they won't leave my init file alone.
 package--init-file-ensured t

 ;; We will run '(package-initialize) in a second
 package-enable-at-startup nil

 package-user-dir (expand-file-name "vendor" user-emacs-directory)
 package-archives '(("org" . "https://orgmode.org/elpa/")
                    ("gnu" . "https://elpa.gnu.org/packages/")
                    ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package-ensure
  :config
  (setq use-package-always-ensure t))


;;; No Littering - Keep extra generated files out of ~/.emacs.d

(use-package no-littering
  :config
  (require 'recentf)

  (push 'no-littering-var-directory recentf-exclude)
  (push 'no-littering-etc-directory recentf-exclude)

  (setq
   custom-file (no-littering-expand-etc-file-name "custom.el")

   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))


;;; User defuns

(defmacro user/filter (fn list)
  "Filters LIST by predicate function FN."
  `(delq nil (mapcar (lambda (l) (and (funcall ,fn l) l)) ,list)))

(defun user/match-paren (arg)
  "Jump to matching parenthesis for ARG at point."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

(defun user/indent-whole-buffer ()
  "Indent Whole Buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))
  ;; (untabify (point-min) (point-max)))

(defun user/kill-current-buffer ()
  "Kill the current buffer without prompting the user."
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
  "Deserialize the FILE at PATH into an s-expr."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer)))
    '()))

(defun user/smart-indent ()
  "Indent region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(defun user/kill-scratch-buffer ()
  ""
  (with-current-buffer "*scratch*"
    (delete-region (point-min) (point-max)))
  nil)

(defun user/other-window-reverse (count &optional all-frames)
  ""
  (interactive "p")
  (let ((count (* -1 count)))
    (other-window count all-frames)))

;;; config/emacs/00_bootstrap.elisp ends here
