;;; config/20_global.el --- global configurations

;; Copyright (C) 2012-2020 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:


;;; Requires


;;; Global setq

(setq

 ;; Global settings
 require-final-newline t

 ;; auto-save settings
 auto-save-list-file-prefix (concat user/recovery-directory "/")
 auto-save-file-name-transforms `((".*" ,(concat user/autosave-directory "/\\1") t))

 ;; backup file settings
 backup-by-copying t
 backup-directory-alist `((".*" . ,(concat user/backup-directory "/")))
 delete-old-versions t
 kept-new-versions 3
 kept-old-versions 2
 version-control t

 ;; save place file
 save-place-file (expand-file-name "saved-places" user/emacs-data-directory)

 ;; Enable all of the disabled functions
 disabled-command-function nil

 ;; TRAMP settings
 tramp-default-method "ssh"
 tramp-terminal-type "tramp"
 tramp-use-ssh-controlmaster-options nil
 tramp-persistency-file-name
 (expand-file-name "tramp-connection-history" user/emacs-data-directory)

 ;; Bookmark settings
 bookmark-default-file (expand-file-name "bookmarks" user/emacs-data-directory)

 ;; URL configuration directory
 url-configuration-directory (expand-file-name "url" user/emacs-data-directory)

 ;; Initialize global network-security environment values
 nsm-settings-file (expand-file-name "network-security.data" user/emacs-data-directory)

 ;; Setup frame values
 initial-frame-alist '((vertical-scroll-bars . nil))
 default-frame-alist '((font . "Liberation Mono-11")
                       (vertical-scroll-bars . nil)
                       (tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (fullscreen . nil))

 ;; Scratch buffer settings
 initial-scratch-message ""
 initial-major-mode 'org-mode

 ;; Setup eshell
 eshell-directory-name user/eshell-directory

 ;; uniquify
 uniquify-after-kill-buffer-p nil
 uniquify-ask-about-buffer-names-p nil
 uniquify-buffer-name-style 'forward
 uniquify-ignore-buffers-re ""
 uniquify-min-dir-content 0
 uniquify-separator "|"
 uniquify-strip-common-suffix nil
 uniquify-trailing-separator-p nil

 ;; recentf
 recentf-save-file user/recentf-save-file
 )


;;; Global execs

;; make yes or no questions sensible
(fset 'yes-or-no-p 'y-or-n-p)

;; Remove GUI items when they exist
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(pending-delete-mode t)
(show-paren-mode t)

;; Prioritize utf-8
(prefer-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)

;; Make scratch magical (WIP)
;; (with-current-buffer "*scratch*"
;;   (add-hook 'kill-buffer-query-functions 'user/kill-current-buffer t t))

;; Remove trailing whitespace on save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Enable eldoc mode
(add-hook 'racer-mode-hook 'eldoc-mode)

;;; config/20_global.el ends here
