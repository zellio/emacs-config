
;;; org-mode.el --- org-mode Configuration

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

;; patch for current version of org as org-agenda-filter is depricaited
;; but sill used
(setq org-agenda-filter 'org-agenda-tag-filter)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-M-RET-may-split-line nil)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-iswitchb)

(setq org-log-done t)

(setq org-agenda-files
      (append
       '("~/.emacs.d/org/school.org"
         "~/.emacs.d/org/work.org")
       (delq nil
             (mapcar
              (lambda (s)
                (if (string= "." (substring s 0 1)) nil
                  (concat "~/projects/" s "/" s ".org")))
              (directory-files "~/projects")))))


(setq org-default-notes-file (expand-file-name "~/.emacs.d/org/notes.org"))
(define-key global-map "\C-cr" 'org-capture)

(setq org-directory "~/.emacs.d/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/.emacs.d/org/mobile.org")

(setq org-todo-keywords
      '((sequence "TODO" "INPR" "WAIT" "DONE")))

(setq org-todo-keyword-faces
      '(("INPR"  . (:foreground "steelblue" :weight bold))
        ("WAIT"  . (:foreground "goldenrod" :weight bold))
        ("PROJECT"  . (:foreground "steelblue" :weight bold))
        ("MAYBE"  . (:foreground "dimgrey" :weight bold)) ))
