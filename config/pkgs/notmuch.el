
;;; notmuch.el --- notmuch mode / email config

;; Copyright (C) 2012-2015 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'notmuch)

(defvar user:notmuch--structural-tags
  '(inbox archive draft send spam deleted)
  "")

(defun user:notmuch--strip-structural-tags ()
  ""
  (notmuch-search-tag
   (mapcar #'(lambda (sym) (format "-%s" sym))
		   user:notmuch--structural-tags)))

(defun user:notmuch--change-structural-tag (tag)
  ""
  (if (not (member tag user:notmuch--structural-tags))
	  (error "%s is not a structural tag")
	(user:notmuch--strip-structural-tags)
	(notmuch-search-tag (list (format "+%s" tag)))))

(dolist (structural-tag user:notmuch--structural-tags)
  (eval
   `(defun ,(intern (format "user:notmuch-set-structural-tag-%s" structural-tag)) ()
	  ""
	  (interactive)
	  (user:notmuch--change-structural-tag (quote ,structural-tag)))
   ))

(defun user:notmuch-toggle-unread-tag ()
  "toggle archive tag for thread"
  (interactive)
  (if (member "unread" (notmuch-search-get-tags))
	  (notmuch-search-tag '("-unread"))
	(notmuch-search-tag '("+unread"))))

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
 mime-edit-pgp-signers '("B0E3D275")
 mime-edit-pgp-encrypt-to-self t

 mml2015-encrypt-to-self t
 mml2015-sign-with-sender t

 notmuch-search-oldest-first nil
 notmuch-show-all-multipart/alternative-parts nil
 notmuch-crypto-process-mime t
 notmuch-always-prompt-for-sender t
 notmuch-fcc-dirs '(("zacharyelliott1@gmail.com" . "gmail.com/zacharyelliott1/[Gmail]/.Sent Mail")
                    (".*@nycresistor.com" . "nycresistor.com/zellio/[Gmail]/.Sent Mail")
					(".*@zell.io" . "zell.io/contact/sent")
					(".*" . "zell.io/contact/sent"))

 notmuch-show-indent-messages-width 2
 notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
						  (:name "unread" :query "tag:unread" :key "u")
						  (:name "flagged" :query "tag:flagged" :key "f")
						  (:name "sent" :query "tag:sent" :key "t")
						  (:name "drafts" :query "tag:draft" :key "d")
						  (:name "archive" :query "tag:archive" :key "a")
						  (:name "all mail" :query "*" :key "a"))

 sendmail-program "/usr/bin/msmtp"

 user-full-name "Zachary Elliott"
 ;; user-mail-address "contact@zell.io"
 )

(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

(define-key notmuch-search-mode-map "g" 'notmuch-refresh-this-buffer)
(define-key notmuch-hello-mode-map "g" 'notmuch-refresh-this-buffer)

(define-key notmuch-search-mode-map "d" 'user:notmuch-set-structural-tag-deleted)
(define-key notmuch-show-mode-map "d" 'user:notmuch-set-structural-tag-deleted)

(define-key notmuch-search-mode-map "a" 'user:notmuch-set-structural-tag-archive)
(define-key notmuch-show-mode-map "a" 'user:notmuch-set-structural-tag-archive)

;; end of notmuch.el
