
;;; global.el --- Global Configurations

;; Copyright (C) 2012,2013 Zachary Elliott
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

;; Directory to store emacs generated files / data
(make-directory "~/.emacs.d/data" t)
(make-directory "~/.emacs.d/data/autosave/" t)
(make-directory "~/.emacs.d/data/backup/"   t)
(make-directory "~/.emacs.d/data/recovery/" t)

;; Set up auto-save file location
(setq
 auto-save-list-file-prefix     "~/.emacs.d/data/recovery/"
 auto-save-file-name-transforms '((".*" "~/.emacs.d/data/autosave/\\1" t)))

;; Setup backup file location
(setq
 backup-by-copying      t
 backup-directory-alist '((".*" . "~/.emacs.d/data/backup/"))
 delete-old-versions    t
 kept-new-versions      3
 kept-old-versions      2
 version-control        t)

;; Set save-place file location
(setq save-place-file "~/.emacs.d/data/saved-places")

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(pending-delete-mode t)
(show-paren-mode     t)
(tool-bar-mode       0)
(scroll-bar-mode    -1)
(menu-bar-mode      -1)

;; end of global.el
