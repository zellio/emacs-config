;;; config/emacs/global.el --- global configurations

;; Copyright (C) 2012-2016 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq-default
 inhibit-startup-message        t
 default-tab-width              4
 c-basic-offset                 4
 kill-whole-line                t
 truncate-partial-width-windows nil
 fill-column                    78
 indicate-empty-lines           t
 line-number-mode               t
 column-number-mode             t
 visible-bell                   t
 truncate-lines                 t
 require-final-newline          t)

(let ((default-directory user/emacs-data-directory))
  (setq
   user/autosave-directory (expand-file-name "autosave")
   user/backup-directory   (expand-file-name "backup")
   user/recovery-directory (expand-file-name "recovery")))

;; Set up auto-save file location
(setq
 auto-save-list-file-prefix
 (concat user/recovery-directory "/")

 auto-save-file-name-transforms
 `((".*" ,(concat user/autosave-directory "/\\1") t)))

;; Setup backup file location
(setq
 backup-by-copying      t
 backup-directory-alist `((".*" . ,(concat user/backup-directory "/")))
 delete-old-versions    t
 kept-new-versions      3
 kept-old-versions      2
 version-control        t)

;; Set save-place file location
(setq
 save-place-file (expand-file-name "saved-places" user/emacs-data-directory))

(fset 'yes-or-no-p 'y-or-n-p)

(setq require-final-newline t)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(pending-delete-mode t)
(show-paren-mode     t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; config/emacs/global.el ends here
