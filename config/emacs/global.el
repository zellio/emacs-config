
;;; global.el --- Global Configurations

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq-default
 inhibit-startup-message        t
 default-tab-width              2
 c-basic-offset                 4
 kill-whole-line                t
 truncate-partial-width-windows nil
 fill-column                    78
 indicate-empty-lines           t
 line-number-mode               t
 column-number-mode             t
 visible-bell                   t)

(defcustom user-emacs-data-directory
  (expand-file-name "data" user-emacs-directory)
  ""
  :type 'string)

(let ((default-directory user-emacs-data-directory))
  (setq
   user-data-directory     (expand-file-name "")
   user-autosave-directory (expand-file-name "autosave")
   user-backup-directory   (expand-file-name "backup")
   user-recovery-directory (expand-file-name "recovery")))

(make-directory user-data-directory t)
(make-directory user-autosave-directory t)
(make-directory user-backup-directory t)
(make-directory user-recovery-directory t)

;; Set up auto-save file location
(setq
 auto-save-list-file-prefix
 (concat user-recovery-directory "/")

 auto-save-file-name-transforms
 `((".*" ,(concat user-autosave-directory "/\\1") t)))

;; Setup backup file location
(setq
 backup-by-copying      t
 backup-directory-alist `((".*" . ,(concat user-backup-directory "/")))
 delete-old-versions    t
 kept-new-versions      3
 kept-old-versions      2
 version-control        t)

;; ;; Set save-place file location
(setq save-place-file (expand-file-name "saved-places" user-data-directory))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(pending-delete-mode t)
(show-paren-mode     t)
(tool-bar-mode       0)
(scroll-bar-mode    -1)
(menu-bar-mode      -1)

;; end of global.el