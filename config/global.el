
;;; global.el --- Global Configurations

;; Copyright (C) 2012 Zachary Elliott
;;
;; Authors: Zachary Elliott <ZacharyElliott1@gmail.com>
;; URL:
;; Version: 1.0.0
;; Keywords:

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(setq-default
 inhibit-startup-message        t
 default-tab-width              2
 c-basic-offset                 4
 kill-whole-line                t
 truncate-partial-width-windows nil
 fill-column                    80
 indicate-empty-lines           t
 line-number-mode               t
 column-number-mode             t
 visible-bell                   t)


;; make sure we have a place to store data
(make-directory "~/.emacs.d/autosave/" t)
(make-directory "~/.emacs.d/backup/"   t)

;; Set up auto-save file location
(setq
 auto-save-list-file-prefix     "~/.emacs.d/recovery/"
 auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/\\1" t)))

;; Setup backup file location
(setq
 backup-by-copying      t
 backup-directory-alist '((".*" . "~/.emacs.d/backup/"))
 delete-old-versions    t
 kept-new-versions      3
 kept-old-versions      2
 version-control        t)


(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(pending-delete-mode t)
(show-paren-mode     t)
(tool-bar-mode       0)
(scroll-bar-mode    -1)
