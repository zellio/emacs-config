
;;; org.el --- org Configuration

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defcustom user:org-directory
  (expand-file-name "org" user:emacs-data-directory)
  ""
  :type 'string)

(defcustom user:org-daily-notes-directory
  (expand-file-name "daily" user:org-directory)
  ""
  :type 'string)

(defcustom user:org-ticket-directory
  (expand-file-name "ticket" user:org-directory)
  ""
  :type 'string)

(defun user:daily-notes-file-path (&optional date)
  (let ((date (or date (current-time))))
    (expand-file-name
     (format-time-string "%Y%m%d.org" date) user:org-daily-notes-directory)))

(defun user:find-daily-notes (&optional date)
  (interactive
   (list (org-read-date "" 'totime nil nil (current-time) "")))
  (let* ((date (or date (current-time))))
    (find-file (user:daily-notes-file-path date))
    (when (eq 0 (buffer-size))
      (insert (format-time-string "#+TITLE: Notes for %A, %B %e, %Y" date))
      (end-of-buffer)
      (newline)
      (save-buffer)
      (newline))))

(defun user:find-todays-daily-notes ()
  (interactive)
  (user:find-daily-notes (current-time))
  (end-of-buffer))

(defun user:ticket-file-path (ticket-id)
  (expand-file-name
   (format "%s.org" ticket-id) user:org-ticket-directory))

(defun user:find-ticket (ticket-id)
  (interactive "sTicket ID: ")
  (find-file (user:ticket-file-path ticket-id))
  (when (eq 0 (buffer-size))
    (insert
     (format "#+TITLE: Ticket %s\n" ticket-id)
     (format-time-string "#+DATE: %A, %B %-e, %Y" (current-time)))
    (save-buffer)
    (newline)
    (newline)))

(setq
 org-directory             user:org-directory
 org-M-RET-may-split-line  nil
 org-log-done              t
 org-use-speed-commands    t

 org-todo-keywords '((sequence "TODO(t)" "INPR(i)" "|" "DONE(d)")
                     (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "OVER(c@/!)"))

 org-todo-keyword-faces  '(("INPR" . (:foreground "steelblue" :weight bold))
                           ("WAIT" . (:foreground "goldenrod" :weight bold))
                           ("HOLD" . (:foreground "dark orange" :weight bold)))

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

 org-capture-templates '(("t" "todo" entry
                          (file (user:daily-notes-file-path)) "\n* TODO %?\n")
                         ("T" "ticket" entry
                          (file (user:daily-notes-file-path))
                          "\n* TODO [[https://tix.es.its.nyu.edu/Ticket/Display.html?id=%^{Ticket number}][tix/%\\1]] - %^{Title}%((lambda () (org-deadline nil \"+1w\") \"\"))\n")

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
