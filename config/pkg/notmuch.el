;;; config/pkg/notmuch.el --- notmuch mode / email config

;; Copyright (C) 2012-2017 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'notmuch)

(defun user/notmuch-search-toggle-tag (tag)
  ""
  (notmuch-search-tag
   (list (concat (if (member tag (notmuch-search-get-tags)) "-" "+") tag))))

(defun user/notmuch-search-apply-tags-and-advance (&rest tags)
  ""
  (notmuch-search-tag tags)
  (notmuch-search-next-thread))

(defun user/notmuch-search-mark-as-read ()
  ""
  (interactive)
  (user/notmuch-search-apply-tags-and-advance "-unread"))

(defun user/notmuch-search-mark-as-unread ()
  ""
  (interactive)
  (user/notmuch-search-apply-tags-and-advance "+unread"))

(defun user/notmuch-search-mark-as-spam ()
  ""
  (interactive)
  (user/notmuch-search-apply-tags-and-advance "-unread" "-inbox" "+spam"))

(defun user/notmuch-search-archive ()
  ""
  (interactive)
  (user/notmuch-search-apply-tags-and-advance "-unread" "-inbox" "+archive"))

(setq
 mail-interactive t
 mail-specify-envelope-from 'header
 mail-envelope-from 'header
 mail-user-agent 'message-user-agent

 message-sendmail-envelope-from 'header
 message-send-mail-function 'message-send-mail-with-sendmail
 message-sendmail-f-is-evil nil
 message-kill-buffer-on-exit t

 mime-edit-split-message nil
 mime-edit-pgp-encrypt-to-self t

 mml2015-encrypt-to-self t
 mml2015-sign-with-sender t

 notmuch-search-oldest-first nil
 notmuch-show-all-multipart/alternative-parts nil
 notmuch-crypto-process-mime t
 notmuch-always-prompt-for-sender t
 notmuch-fcc-dirs '(("zacharyelliott1@gmail.com" . "\"gmail.com/zacharyelliott1/[Gmail]/.Sent Mail\"")
                    (".*@nycresistor.com" . "\"nycresistor.com/zellio/[Gmail]/.Sent Mail\"")
                    (".*@zell.io" . "zell.io/contact/sent")
                    (".*@xz.je" . "xz.je/contact/sent")
                    (".*" . "zell.io/contact/sent"))

 notmuch-show-indent-messages-width 2
 notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                          (:name "unread" :query "tag:unread" :key "u")
                          (:name "flagged" :query "tag:flagged" :key "f")
                          (:name "sent" :query "tag:sent" :key "t")
                          (:name "drafts" :query "tag:draft" :key "d")
                          (:name "archive" :query "tag:archive" :key "A")
                          (:name "all mail" :query "*" :key "a"))

 sendmail-program "/usr/bin/msmtp"

 user-full-name "Zachary Elliott"
 ;; user-mail-address "contact@zell.io"
 )

(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

(define-key notmuch-search-mode-map "g" 'notmuch-refresh-this-buffer)
(define-key notmuch-hello-mode-map "g" 'notmuch-refresh-this-buffer)

(define-key notmuch-search-mode-map "i" 'user/notmuch-search-mark-as-read)
(define-key notmuch-show-mode-map "i" 'user/notmuch-search-mark-as-read)

(define-key notmuch-search-mode-map "u" 'user/notmuch-search-mark-as-unread)
(define-key notmuch-show-mode-map "u" 'user/notmuch-search-mark-as-unread)

(define-key notmuch-search-mode-map "!" 'user/notmuch-search-mark-as-spam)
(define-key notmuch-show-mode-map "!" 'user/notmuch-search-mark-as-spam)

(define-key notmuch-search-mode-map "e" 'user/notmuch-search-archive)
(define-key notmuch-show-mode-map "e" 'user/notmuch-search-archive)

(global-set-key (kbd "C-c n") 'notmuch-hello)

(custom-set-faces
 '(notmuch-crypto-part-header ((t (:foreground "deep sky blue"))))
 '(notmuch-hello-logo-background ((t nil)))
 '(notmuch-message-summary-face ((t nil)))
 '(notmuch-search-unread-face ((t (:foreground "deep sky blue"))))
 '(notmuch-tag-flagged ((t (:foreground "deep sky blue")))))

;;; config/pkg/notmuch.el ends here
