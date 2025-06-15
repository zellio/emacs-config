;;; emacs.el --- Base emacs config -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Copyright (C) 2012-2025 Zachary Elliott

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>>
;; Version: 0.8.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'rx)
  (require 'no-littering)
  (declare-function no-littering-expand-var-file-name "no-littering")
  (declare-function consult-register-format "consult"))

;;;###autoload
(defmacro user/defun-setq-local-hook (name &rest varlist)
  "Generate a hook named NAME which sets VARLIST locally."
  `(defun ,name ()
     ""
     ,@(mapcar
        (lambda (pair)
          (unless (length= pair 2)
            (error "[setq-local-hook] PAIRS must have an even number of variable/value members"))
          `(set (make-local-variable (quote ,(car pair))) ,(cadr pair)))
        (seq-partition varlist 2))))

;;;###autoload
(defmacro user/defun-enable-mode (mode)
  "Generate a defun named enable-mode which enables MODE."
  (let ((func-name (intern (format "enable-%s" mode))))
    `(defun ,func-name ()
       ""
       (,mode +1))))

;;;###autoload
(defmacro user/defun-disable-mode (mode)
  "Generate a defun named enable-mode which enables MODE."
  (let ((func-name (intern (format "disable-%s" mode))))
    `(defun ,func-name ()
       ""
       (,mode -1))))

(use-package emacs
  :preface
  (defvar user/indent-width 4
    "Default user indentation width.")

  (defsubst user/home-path (name)
    "Path of NAME relative to user HOME."
    (expand-file-name name "~"))

  (defsubst user/expand-user-emacs-directory-file (name)
    ""
    (expand-file-name name user-emacs-directory))

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

  (defun user/scratch-after-save-function ()
    "Respawn scratch buffer after save."
    (interactive)
    (unless (get-buffer "*scratch*")
      (user/scratch-buffer-respawn)))

  :custom
  ;; c source
  (user-full-name "Zachary Elliott")
  (fill-column 78)
  (visible-bell t)
  (truncate-partial-width-windows nil)
  (truncate-lines t)
  (tab-width user/indent-width)
  (use-short-answers t)
  (enable-recursive-minibuffers t)
  (use-dialog-box nil)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; bindings.el
  (mode-line-right-align-edge 'right-margin)

  ;; indent.el
  (standard-indent user/indent-width)
  (tab-always-indent 'complete)
  (tab-first-completion 'eol)

  ;; map-ynp.el
  (read-answer-short t)

  ;; mule-cmds.el
  (input-method-verbose-flag 'complex-only)
  (current-language-environment "English")

  ;; paragraph.el
  (sentence-end-double-space nil)

  ;; startup.el
  (initial-buffer-choice t)
  (inhibit-startup-screen t)
  (inhibit-startup-echo-area-message nil)
  (inhibit-startup-buffer-menu t)
  (initial-major-mode 'org-mode)
  (user-mail-address "contact@zell.io")
  (initial-scratch-message "")
  (default-frame-alist (cons '(font . "JuliaMono-18") initial-frame-alist))

  :config
  (setq
   frame-title-format '("Emacs " emacs-version)
   read-process-output-max #x10000)

  (prefer-coding-system 'utf-8)
  (prefer-coding-system 'utf-8-unix)

  (add-to-list 'kill-buffer-query-functions #'user/scratch-kill-buffer-query-function t)
  (add-hook 'after-save-hook #'user/scratch-after-save-function))

(use-package align
  :custom (align-indent-before-aligning t))

(use-package ansi-color
  :hook (shell-mode . ansi-color-for-comint-mode-on)
  :custom (ansi-color-bold-is-bright t))

(use-package apropos
  :custom
  (apropos-sort-by-scores t)
  (apropos-documentation-sort-by-scores t))

(use-package autorevert
  :preface (user/defun-enable-mode global-auto-revert-mode)
  :hook (emacs-startup . enable-global-auto-revert-mode)
  :custom
  (auto-revert-mode-text "")
  (auto-revert-tail-mode-text "")
  (global-auto-revert-mode-text "")
  (auto-revert-avoid-polling t))

(use-package bookmark
  :custom
  (bookmark-use-annotations t)
  (bookmark-sort-flag 'last-modified))

(use-package comint
  :custom
  (comint-prompt-read-only t)
  (comint-highlight-input nil)
  (comint-scroll-to-bottom-on-input 'this)
  (comint-move-point-for-output 'others)
  (comint-buffer-maximum-size 10240)
  (comint-input-ring-size 512))

(use-package completion
  :custom (save-completions-retention-time 672))

(use-package dabbrev
  :custom
  (dabbrev-case-fold-search t)
  (dabbrev-upcase-means-case-search t)
  (dabbrev-case-replace nil))

(use-package delsel
  :preface (user/defun-enable-mode delete-selection-mode)
  :hook (emacs-startup . enable-delete-selection-mode))

(use-package dired
  :custom
  (dired-hide-details-hide-symlink-targets nil)
  (dired-hide-details-hide-information-lines nil)
  (dired-auto-revert-buffer t)
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'top))

(use-package dired-aux
  :custom
  (dired-confirm-shell-command nil)
  (dired-backup-overwrite 'always)
  (dired-create-destination-dirs 'always)
  (dired-create-destination-dirs-on-trailing-dirsep t))

(use-package dired-x
  :custom
  (dired-bind-vm t)
  (dired-x-hands-off-my-keys t))

(use-package display-line-numbers
  :preface (user/defun-enable-mode display-line-numbers-mode)
  :hook (prog-mode . enable-display-line-numbers-mode)
  :custom (display-line-numbers-width-start 100))

(use-package files
  :after no-littering
  :hook (before-save . delete-trailing-whitespace)
  :custom
  (backup-by-copying t)
  (backup-by-copying-when-linked t)
  (dired-kept-versions 3)
  (delete-old-versions t)
  (kept-old-versions 3)
  (kept-new-versions 3)
  (require-final-newline 'visit-save)
  (auto-save-default t)
  (auto-save-file-name-transforms
   `((,(rx line-start (zero-or-more any) line-end)
      ,(no-littering-expand-var-file-name "auto-save/")
      t)))
  (view-read-only t)
  (large-file-warning-threshold 10485760)
  (safe-local-variable-values
   '((lexical-binding . t)
     (lexical-binding . nil)))
  (backup-directory-alist
   `((,(rx any) . ,(no-littering-expand-var-file-name "backup/"))))
  (copy-directory-create-symlink t)
  (confirm-kill-processes nil))

(use-package ielm
  :custom
  (ielm-noisy nil)
  (ielm-prompt "IELM> "))

(use-package kmacro
  :custom (kmacro-ring-max 16))

(use-package menu-bar
  :preface (user/defun-disable-mode menu-bar-mode)
  :hook (emacs-startup . disable-menu-bar-mode))

(use-package minibuffer
  :custom (completion-styles '(flex basic partial-completion emacs22)))

(use-package mouse
  :preface (user/defun-enable-mode context-menu-mode)
  :hook (emacs-startup . enable-context-menu-mode))

(use-package mwheel
  :custom
  (mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
  (mouse-wheel-progressive-speed nil))

(use-package newcomment
  :custom (comment-empty-lines 'eol))

(use-package novice
  :config (setq disabled-command-function nil))

(use-package paren
  :preface
  (user/defun-disable-mode show-paren-mode)
  (user/defun-enable-mode show-paren-local-mode)

  (defun user/show-paren-data-function-around (orig-fun)
    "Corrects show-paren functionality."

    (cond ((looking-at (rx (syntax close-parenthesis)))
           (save-excursion (forward-char 1) (funcall orig-fun)))
          (t (funcall orig-fun))))

  :hook
  (emacs-startup . disable-show-paren-mode)
  (prog-mode . enable-show-paren-local-mode)

  :custom
  (show-paren-style 'mixed)
  (show-paren-delay 0.1)
  (show-paren-context-when-offscreen t)

  :config
  ;; Force show-paren to act sanely
  (advice-add show-paren-data-function :around #'user/show-paren-data-function-around))

(use-package recentf
  :after no-littering
  :preface (user/defun-enable-mode recentf-mode)
  :hook (emacs-startup . enable-recentf-mode)
  :general ("C-x C-r" 'recentf-open-files)
  :custom
  (recentf-max-saved-items 256)
  (recentf-arrange-by-rules-min-items 1)
  (recentf-auto-cleanup 'mode)
  (recentf-menu-filter 'recentf-arrange-by-mode)

  :config
  (push 'no-littering-var-directory recentf-exclude)
  (push 'no-littering-etc-directory recentf-exclude))

(use-package register
  :after consult
  :custom (register-preview-delay 0.5)
  :config
  (setq-default
   register-preview-function #'consult-register-format))

(use-package replace
  :custom
  (case-replace t)
  (replace-char-fold t)
  (query-replace-skip-read-only t))

(use-package reveal
  :preface (user/defun-enable-mode global-reveal-mode)
  :hook (emacs-startup . enable-global-reveal-mode))

(use-package savehist
  :hook (emacs-startup . savehist-mode))

(use-package saveplace
  :preface (user/defun-enable-mode save-place-mode)
  :hook (emacs-startup . enable-save-place-mode)
  :custom (save-place-limit 512))

(use-package scroll-bar
  :preface (user/defun-disable-mode scroll-bar-mode)
  :hook (emacs-startup . disable-scroll-bar-mode))

(use-package select
  :custom (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package shell
  :custom (shell-has-auto-cd t))

(use-package simple
  :preface
  (user/defun-enable-mode line-number-mode)
  (user/defun-enable-mode column-number-mode)
  (user/defun-disable-mode overwrite-mode)
  (user/defun-disable-mode auto-fill-mode)

  :hook
  (emacs-startup . enable-line-number-mode)
  (emacs-startup . enable-column-number-mode)
  (emacs-startup . disable-overwrite-mode)
  (emacs-startup . indent-tabs-mode)
  (text-mode . disable-auto-fill-mode)

  :custom
  (goto-line-history-local t)
  (kill-ring-max 256)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (kill-append-merge-undo t)
  (yank-pop-change-selection nil)
  (kill-whole-line t)
  (track-eol t)
  (indent-tabs-mode nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package time-stamp
  :custom (time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S%Z"))

(use-package tool-bar
  :preface (user/defun-disable-mode tool-bar-mode)
  :hook (emacs-startup . disable-tool-bar-mode))

(use-package treesit
  :after no-littering
  :commands treesit--install-language-grammar-1
  :preface
  (defcustom user/treesit-extra-load-path
    (no-littering-expand-var-file-name "tree-sitter")
    "Treesit lib directory override."
    :type 'string
    :group 'user)

  :config
  (setq-default
   treesit-extra-load-path (list user/treesit-extra-load-path)

   treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (gotmpl "https://github.com/zellio/tree-sitter-gotmpl")
     (helm "https://github.com/ngalaiko/tree-sitter-go-template" "master" "dialects/helm/src")
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (nix "https://github.com/nix-community/tree-sitter-nix")
     (proto "https://github.com/coder3101/tree-sitter-proto")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (terraform "https://github.com/MichaHoffmann/tree-sitter-hcl" "main" "dialects/terraform/src")
     (toml "https://github.com/ikatyang/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (pcase-dolist (`(,language . ,recipe) treesit-language-source-alist)
    (unless (treesit-language-available-p language)
      (condition-case error
          (progn
            (apply #'treesit--install-language-grammar-1 user/treesit-extra-load-path language recipe)
            (pcase (treesit-language-available-p language t)
              (`(nil . (not-found . ,_error-messages))
               (let ((message (format "Failed to locate installed language grammar for %s" language)))
                 (display-warning 'user/treesit message)))
              (`(nil . (symbol-error . ,_error-message))
               (let ((message (format "Bad language grammar library installed for %s" language)))
                 (display-warning 'user/treesit message)))
              (`(nil . (version-mismatch ,_error-message))
               (let ((message (format "LibC version mismatch in installed language grammar for %s" language)))
                 (display-warning 'user/treesit message)))))
        (error
         (let ((message (format "Failed to install language grammar for %s: %s" language error)))
           (display-warning 'user/treesit message)))))))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-after-kill-buffer-p t)
  (uniquify-min-dir-content 0)
  (uniquify-trailing-separator-p t)
  (uniquify-strip-common-suffix t))

(use-package windmove
  :hook (emacs-startup . windmove-default-keybindings)
  :custom (windmove-wrap-around t))

;;; emacs-lisp

(use-package advice
  :custom (ad-default-compilation-action 'like-original))

(use-package backtrace
  :custom (backtrace-line-length nil))

(use-package eldoc
  :preface (user/defun-enable-mode global-eldoc-mode)
  :hook (emacs-startup . enable-global-eldoc-mode)
  :custom (eldoc-minor-mode-string nil))

(use-package package
  :custom
  (package-load-list nil)
  (package-archives nil)
  (package-user-dir (user/expand-user-emacs-directory-file "vendor"))
  (package-selected-packages nil))

(use-package re-builder
  :custom (reb-re-syntax 'rx))

;;; eshell

(use-package em-banner
  :custom (eshell-banner-message "Emacs Shell\n\n"))

(use-package em-dirs
  :custom
  (eshell-pushd-dunique t)
  (eshell-dirtrack-verbose nil))

(use-package em-glob
  :custom
  (eshell-glob-include-dot-dot nil)
  (eshell-glob-case-insensitive nil))

(use-package em-hist
  :custom
  (eshell-history-size (* 1024 1024))
  (eshell-hist-ignoredups t))

(use-package esh-ext
  :custom (eshell-command-interpreter-max-length 1024))

(use-package esh-mode
  :custom
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-scroll-to-bottom-on-output 'others)
  (eshell-buffer-maximum-lines 5120))

;;; net

(use-package tramp
  :after no-littering
  :custom
  (tramp-mode t)
  (tramp-verbose 4)
  (tramp-auto-save-directory (no-littering-expand-var-file-name "backup-tramp/"))
  (tramp-terminal-type "tramp")
  (tramp-connection-timeout 30))

;;; term

(use-package term
  :custom
  (term-set-terminal-size t)
  (term-scroll-to-bottom-on-output 'this)
  (term-buffer-maximum-size 10240))

(use-package term/ns-win
  :if (eq system-type 'darwin)
  :config
  (setq
   ns-command-modifier 'super
   ns-right-command-modifier 'super
   ns-option-modifier 'meta
   ns-right-option-modifier 'meta))

;;; url

(use-package url-history
  :after no-litering
  :custom
  (url-history-track t)
  (url-history-file (no-littering-expand-var-file-name "url/history")))

(use-package url-vars
  :custom
  (url-automatic-caching t)
  (url-privacy-level '(email os emacs))
  (url-max-redirections 16))

(provide 'config/emacs)

;;; emacs.el ends here
