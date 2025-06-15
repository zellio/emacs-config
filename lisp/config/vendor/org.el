;;; org.el --- Vendored org mode config -*- lexical-binding: t -*-

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
  (require 'rx))

(use-package org
  :mode ((rx ".org" line-end) . org-mode)
  :preface
  (defcustom user/org-directory
    (no-littering-expand-var-file-name "org")
    "Org file root directory."
    :type 'string
    :group 'user)

  (defcustom user/org-journal-file
    (expand-file-name "journal.org" user/org-directory)
    "Org journal file."
    :type 'string
    :group 'user)

  (defcustom user/org-incident-file
    (expand-file-name "incident.org" user/org-directory)
    ""
    :type 'string
    :group 'user)

  (defcustom user/org-default-notes-file
    (expand-file-name "notes.org" user/org-directory)
    "Org notest files."
    :type 'string
    :group 'user)

  (defun user/org-capture-template-todo ()
    (let ((lines '("* TODO %^{Description}%? %^g" ":LOGBOOK:"
                   "- Created                              %U"
                   ":END:")))
      (mapconcat #'identity lines "\n  ")))

  (defun user/org-capture-template-work-task ()
    (let ((lines '("* TODO %^{Type|CORE|SRE|DEVX|POMO}-%^{Ticket number} - %^{Description}%?"
                   ":PROPERTIES:"
                   ":LINK:     https://formationbio.atlassian.net/browse/%\\1-%\\2"
                   ":END:"
                   ":LOGBOOK:"
                   "- Created                              %U"
                   ":END:")))
      (mapconcat #'identity lines "\n  ")))

  (defun user/org-capture-template-oncall-task ()
    (let ((lines '("* TODO %^{Incident Id} - %^{Description}%?"
                   ":PROPERTIES:"
                   ":LINK:     https://10gen.pagerduty.com/incidents/%\\1"
                   ":END:"
                   ":LOGBOOK:"
                   "- Created                              %U"
                   ":END:")))
      (mapconcat #'identity lines "$\n  ")))

  (defun user/org-capture-template-journal ()
    (let ((lines '("* %^{Description}"
                   ":LOGBOOK:"
                   "- Captured %U"
                   ":END:"
                   "%?")))
      (mapconcat #'identity lines "\n  ")))

  :general
  ("C-c o a" 'org-agenda)
  ("C-c o r" 'org-capture)

  :hook
  (org-mode . (lambda ()
                (local-set-key [(control return)] 'org-insert-heading-after-current)))

  :custom
  (org-directory user/org-directory)
  (org-default-notes-file user/org-default-notes-file)
  (org-agenda-files (list user/org-directory))
  (org-M-RET-may-split-line nil)
  (org-log-done nil)
  (org-log-into-drawer t)
  (org-use-speed-commands t)
  (org-id-track-globally t)
  (org-highest-priority ?A)
  (org-default-priority ?C)
  (org-lowest-priority ?Z)

  (org-todo-keywords
   '((sequence "TODO(t)" "INPR(i!)" "|" "DONE(d@)")
     (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "OVER(c@/!)" "PHONE" "MEETING")
     (sequence "TODO" "INPR" "REVW" "TEST" "|" "DONE" "ARCH" "WONT")))

  (org-todo-keyword-faces
   '(("INPR" . (:foreground "steelblue" :weight bold))
     ("WAIT" . (:foreground "goldenrod" :weight bold))
     ("HOLD" . (:foreground "dark orange" :weight bold))
     ("MEETING" . (:foreground "forest green" :weight bold))
     ("PHONE" . (:foreground "forest green" :weight bold))))

  (org-todo-state-tags-triggers
   '(("OVER" ("OVER" . t))
     ("WAIT" ("WAIT" . t))
     ("HOLD" ("WAIT") ("HOLD" . t))
     (done ("WAIT") ("HOLD"))
     ("TODO" ("WAIT") ("OVER") ("HOLD"))
     ("INPR" ("WAIT") ("OVER") ("HOLD"))
     ("DONE" ("WAIT") ("OVER") ("HOLD"))))

  (org-capture-templates
   '(("t" "Todo"
      entry (file org-default-notes-file)
      (function user/org-capture-template-todo)
      :empty-lines-after 1
      :prepend t)

     ("w" "Work Todo"
      entry (file org-default-notes-file)
      (function user/org-capture-template-work-task)
      :empty-lines-after 1
      :prepend t)

     ("i" "Pagerduty Incident"
      entry (file user/org-incident-file)
      (function user/org-capture-template-oncall-task)
      :empty-lines-after 1
      :prepent t)

     ("j" "Journal"
      entry (file+olp+datetree user/org-journal-file)
      (function user/org-capture-template-journal)
      :empty-lines-before 1))
   ))

(provide 'config/vendor/org)

;;; org.el ends here
