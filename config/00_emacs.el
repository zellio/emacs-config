;;; 10_emacs.el --- Base emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(use-package emacs
  :preface
  (defvar user/home-directory
    (or (getenv "HOME") (expand-file-name "~")))

  (defun user/home-path (name)
    ""
    (expand-file-name name "~"))

  (defvar user/indent-width 4)

  (defun user/load-environment (env-alist)
    "Load shell environment snapshot into Emacs."
    (dolist (env-pair env-alist)
      (let ((name (car env-pair)) (value (cdr env-pair)))
        (when (string-equal name "PATH")
          (setq
           exec-path (append (list exec-directory) (parse-colon-path value)))
          (setq-default
           eshell-path-env (mapconcat 'identity exec-path ":")))
        (setenv name value))))

  (defun user/match-paren (_)
    "Go to the matching paren if on a paren"
    (interactive "p")
    (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s)") (forward-char 1) (backward-list 1))))

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
         ";; -*- mode: lisp; coding: utf-8; -*-\n"
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

  (defun user/upper-camel-case-region (beg end)
    ""
    (interactive "*r")
    (replace-region-contents
     beg end
     (lambda () (s-upper-camel-case (buffer-substring beg end)))))

  (defun user/snake-case-region (beg end)
    ""
    (interactive "*r")
    (replace-region-contents
     beg end
     (lambda () (s-snake-case (buffer-substring beg end)))))

  (defmacro user/add-eglot-workspace-config (server conf-plist)
    ""
    `(with-eval-after-load 'eglot
       (setq-default
        eglot-workspace-configuration
        (plist-put eglot-workspace-configuration ,server ',conf-plist))))

  (defun user/scratch-buffer-respawn ()
    "Re-initialize contents of the scratch buffer instead of destroying it"
    (interactive)
    (with-current-buffer (get-buffer-create "*scratch*")
      (delete-region (point-min) (point-max))
      (insert initial-scratch-message)
      (funcall initial-major-mode)))

  (defun user/scratch-kill-buffer-query-function (&optional slay)
    ""
    (interactive "P")
    (or (not (string= (buffer-name) "*scratch*"))
        (and slay
             (let ((kill-buffer-query-functions kill-buffer-query-functions))
               (remove-hook 'kill-buffer-query-functions 'user/scratch-kill-buffer-query-function)
               (kill-buffer "*scratch*")))
        (user/scratch-buffer-respawn)))

  (defun user/mask-auto-mode-alist (source-mode mask-mode)
    ""
    (mapcar
     (lambda (auto-mode-cell)
       (let ((matcher (car auto-mode-cell)) (mode (cdr auto-mode-cell)))
         (cons matcher (if (eq mode source-mode) mask-mode mode))))
     auto-mode-alist))

  :config
  (setq
   user-full-name "Zachary Elliott"
   frame-title-format '("Emacs " emacs-version)

   initial-buffer-choice t
   inhibit-startup-screen t
   inhibit-startup-echo-area-message nil
   inhibit-startup-buffer-menu t
   initial-major-mode 'org-mode
   initial-scratch-message "")

  (setq-default
   default-tab-width user/indent-width
   truncate-partial-width-windows nil
   fill-column 78
   visible-bell t
   truncate-lines t
   indent-tabs-mode nil
   standard-indent user/indent-width
   tab-width user/indent-width
   x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; make yes or no questions sensible
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Prioritize utf-8
  (prefer-coding-system 'utf-8)
  (prefer-coding-system 'utf-8-unix)

  ;; OSX Specific configs
  (when (eq system-type 'darwin)
    (setq
     mac-option-modifier 'meta
     mac-command-modifier 'super))

  (add-to-list 'kill-buffer-query-functions 'user/scratch-kill-buffer-query-function))

(use-package diminish
  :ensure t)

;;; 10_emacs.el ends here
