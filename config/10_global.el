;;; -*- lexical-bindings: t -*-

;;; config/20_global.el --- global configurations

;; Copyright (C) 2012-2022 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:


;;; Requires

(use-package emacs
  :ensure nil
  :preface
  (defvar user/indent-width 4)

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
        (indent-region (region-beginning) (region-end))
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

  (defun user/split-window-below-and-switch ()
    "Split window below and switch to it."
    (interactive)
    (split-window-below)
    (other-window 1))

  (defun user/split-window-right-and-switch ()
    "Split window below and switch to it."
    (interactive)
    (split-window-right)
    (other-window 1))

  :hook (before-save . delete-trailing-whitespace)
  :config
  (setq
   user-full-name "Zachary H. Elliott"
   frame-title-format '("Emacs " emacs-version)

   ;; backup file settings
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 3
   kept-old-versions 2
   version-control t

   ;; Enable all of the disabled functions
   disabled-command-function nil

   ;; Disable startup screen
   inhibit-startup-screen t

   ;; Scratch buffer settings
   initial-scratch-message ""
   initial-major-mode 'org-mode)

  (setq-default
   inhibit-startup-message t
   default-tab-width 4
   kill-whole-line t
   truncate-partial-width-windows nil
   fill-column 78
   indicate-empty-lines t
   visible-bell t
   truncate-lines t
   require-final-newline t
   indent-tabs-mode nil
   standard-indent user/indent-width
   tab-width user/indent-width
   x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; make yes or no questions sensible
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Prioritize utf-8
  (prefer-coding-system 'utf-8)
  (prefer-coding-system 'utf-8-unix))


;;; config/20_global.el ends here
