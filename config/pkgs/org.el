
;;; org.el --- org Configuration

;; Copyright (C) 2012-2015 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defcustom user:org-directory
  (expand-file-name "org" user:emacs-data-directory)
  ""
  :type 'string)

(defcustom user:org-journal-file
  (expand-file-name "journal.org" user:org-directory)
  ""
  :type 'string)

(defcustom user:org-daily-notes-directory
  (expand-file-name "daily" user:org-directory)
  ""
  :type 'string)

(defun user:daily-notes-file-path (&optional date)
  (let ((date (or date (current-time))))
    (expand-file-name
     (format-time-string "%Y%m%d.org" date) user:org-daily-notes-directory)))

(defun user:find-daily-notes (&optional date)
  (interactive
   (list (org-read-date "" 'totime nil nil (current-time) "")))
  (let* ((date (or date (current-time)))
         (file (user:daily-notes-file-path date)))
    (find-file file)
    (unless (file-readable-p file)
      (insert (format-time-string "#+TITLE: Notes for %A, %B %e, %Y" date))
      (end-of-buffer)
      (newline)
      (save-buffer)
      (newline))))

(defun user:find-todays-daily-notes ()
  (interactive)
  (user:find-daily-notes (current-time))
  (end-of-buffer))

(setq
 org-directory             user:org-directory
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

 org-agenda-files (list user:org-directory
                        user:org-daily-notes-directory)

 org-default-notes-file (expand-file-name "scratch.org" user:org-directory)

 org-capture-templates '(
                         ("t" "TODO"
                          entry (function user:find-daily-notes)
                          "\n* TODO %?\n"
                          :empty-lines 1)

                         ;; -- Job specific --
                         ("T" "Ticket"
                          entry (function user:find-daily-notes)
                          "* TODO [[https://tix.es.its.nyu.edu/Ticket/Display.html?id=%^{Ticket number}][tix/%\\1]] - %^{Title}%((lambda () (org-deadline nil \"+1w\") \"\"))\n"
                          :empty-lines 1)

                         ("j" "Journal"
                          entry (file+datetree user:org-journal-file)
                          "* %<%H:%M> - %?"
                          :empty-lines 1)

                         ;; ("x" "ticket" entry
                         ;;  (file (lambda () (call-interactively #'user:find-ticket))) "* ")

                         )
 )

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c o n") 'user:find-todays-daily-notes)
(global-set-key (kbd "C-c o N") 'user:find-daily-notes)
(global-set-key (kbd "C-c r") 'org-capture)

(make-directory user:org-directory t)
(make-directory user:org-daily-notes-directory t)

(add-hook
 'org-mode-hook
 (lambda ()
   (local-set-key [(control return)] 'org-insert-heading-after-current)))

;;; org.el ends here
