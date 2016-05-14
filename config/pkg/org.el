;;; org.el --- org configuration

;; Copyright (C) 2012-2016 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'org-themis)

(defcustom user/org-directory
  (expand-file-name "org" user/emacs-data-directory)
  ""
  :type 'string)

(defcustom user/org-journal-file
  (expand-file-name "journal.org" user/org-directory)
  ""
  :type 'string)

;; I may want to move this into org-themis.el
(defun user/org-themis-list-project-roots ()
  (mapcar (lambda (x) (cdr (assoc 'root x)))
          (cdr (assoc 'projects org-themis-data))))

(setq
 org-directory             user/org-directory
 org-M-RET-may-split-line  nil
 org-log-done              t
 org-use-speed-commands    t

 org-todo-keywords '((sequence "TODO(t)" "INPR(i)" "|" "DONE(d)")
                     (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "OVER(c@/!)" "PHONE" "MEETING"))

 org-todo-keyword-faces  '(("INPR" . (:foreground "steelblue" :weight bold))
                           ("WAIT" . (:foreground "goldenrod" :weight bold))
                           ("HOLD" . (:foreground "dark orange" :weight bold))
                           ("MEETING" . (:foreground "forest green" :weight bold))
                           ("PHONE" . (:foreground "forest green" :weight bold)))

 org-todo-state-tags-triggers '(("OVER" ("OVER" . t))
                                ("WAIT" ("WAIT" . t))
                                ("HOLD" ("WAIT") ("HOLD" . t))
                                (done ("WAIT") ("HOLD"))
                                ("TODO" ("WAIT") ("OVER") ("HOLD"))
                                ("INPR" ("WAIT") ("OVER") ("HOLD"))
                                ("DONE" ("WAIT") ("OVER") ("HOLD")))

 org-highest-priority ?A
 org-default-priority ?C
 org-lowest-priority  ?Z

 org-agenda-files (append (user/org-themis-list-project-roots)
                          (list user/org-directory))

 org-default-notes-file (expand-file-name "scratch.org" user/org-directory)

 org-capture-templates '(("t" "TODO"
                          entry (file org-default-notes-file)
                          "\n* TODO %?\n"
                          :empty-lines 1)

                         ;; -- org-themis todo capture
                         ("T" "ot:TODO"
                          entry (function org-themis-find-project-scratch)
                          "\n* TODO %?\n"
                          :empty-lines 1)

                         ("j" "Journal"
                          entry (file+datetree user/org-journal-file)
                          "* %<%H:%M> - %?"
                          :empty-lines 1)

                         ;; -- org-themis todo capture
                         ("J" "ot:Journal"
                          entry (function org-themis-find-project-scratch)
                          "* %<%H:%M> - %?"
                          :empty-lines 1))
 )

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "C-c o t") 'org-themis-mode)

(add-hook
 'org-mode-hook
 (lambda ()
   (local-set-key [(control return)] 'org-insert-heading-after-current)))

(dolist
    (mode-hook
     '(org-mode-hook
       org-agenda-mode-hook
       org-capture-mode-hook
       org-remember-mode-hook
       org-src-mode-hook))
  (add-hook mode-hook (lambda () (org-themis-mode t))))

;;; org.el ends here
