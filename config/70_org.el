;;; 70_org.el --- org-mode configuration -*- lexical-binding: t -*-

;; Copyright (C) 2012-2023 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defcustom user/org-directory
  (no-littering-expand-var-file-name "org")
  ""
  :type 'string
  :group 'user)

(defcustom user/org-journal-file
  (expand-file-name "journal.org" user/org-directory)
  ""
  :type 'string
  :group 'user)

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind (("C-c o a" . org-agenda)
         ("C-c o r" . org-capture))

  :hook (org-mode . (lambda ()
                      (local-set-key [(control return)] 'org-insert-heading-after-current)))

  :config
  (setq
   org-directory user/org-directory
   org-M-RET-may-split-line nil
   org-log-done t
   org-use-speed-commands t
   org-id-track-globally t
   org-highest-priority ?A
   org-default-priority ?C
   org-lowest-priority ?Z
   org-agenda-files (list user/org-directory)
   org-default-notes-file (expand-file-name "scratch.org" user/org-directory)

   org-todo-keywords
   '((sequence "TODO(t)" "INPR(i)" "|" "DONE(d)")
     (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "OVER(c@/!)" "PHONE" "MEETING")
     (sequence "TODO" "INPR" "REVW" "TEST" "|" "DONE" "ARCH" "WONT"))

   org-todo-keyword-faces
   '(("INPR" . (:foreground "steelblue" :weight bold))
     ("WAIT" . (:foreground "goldenrod" :weight bold))
     ("HOLD" . (:foreground "dark orange" :weight bold))
     ("MEETING" . (:foreground "forest green" :weight bold))
     ("PHONE" . (:foreground "forest green" :weight bold)))

   org-todo-state-tags-triggers
   '(("OVER" ("OVER" . t))
     ("WAIT" ("WAIT" . t))
     ("HOLD" ("WAIT") ("HOLD" . t))
     (done ("WAIT") ("HOLD"))
     ("TODO" ("WAIT") ("OVER") ("HOLD"))
     ("INPR" ("WAIT") ("OVER") ("HOLD"))
     ("DONE" ("WAIT") ("OVER") ("HOLD")))

   org-capture-templates
   '(("t" "TODO"
      entry (file org-default-notes-file)
      "\n* TODO %?\n"
      :empty-lines 1)

     ("j" "Journal"
      entry (file+datetree user/org-journal-file)
      "* %<%H:%M> - %?"
      :empty-lines 1))))

;;; 70_org.el ends here
