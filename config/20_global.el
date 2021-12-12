;;; config/20_global.el --- global configurations

;; Copyright (C) 2012-2020 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:


;;; Requires

(setq-default
 inhibit-startup-message t
 default-tab-width 4
 c-basic-offset 4
 kill-whole-line t
 truncate-partial-width-windows nil
 fill-column 78
 indicate-empty-lines t
 line-number-mode t
 column-number-mode t
 visible-bell t
 truncate-lines t
 require-final-newline t

 ;; tab controls
 indent-tabs-mode nil
 standard-indent 4
 tab-width 4

 ;; utf-8 defaults
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq
 ;; Global settings
 require-final-newline t

 ;; backup file settings
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 3
 kept-old-versions 2
 version-control t

 ;; Enable all of the disabled functions
 disabled-command-function nil

 ;; Setup frame values
 initial-frame-alist '((vertical-scroll-bars . nil))
 default-frame-alist '((font . "Liberation Mono-18")
                       (vertical-scroll-bars . nil)
                       (tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (fullscreen . nil))

 ;; Disable startup screen
 inhibit-startup-screen t

 ;; Scratch buffer settings
 initial-scratch-message ""
 initial-major-mode 'org-mode)

;; make yes or no questions sensible
(fset 'yes-or-no-p 'y-or-n-p)

;; Prioritize utf-8
(prefer-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;;; config/20_global.el ends here
