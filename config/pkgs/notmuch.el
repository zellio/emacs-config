
;;; notmuch.el --- notmuch mode / email config

;; Copyright (C) 2012-2015 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'notmuch)

(defun user:notmuch-toggle-delete-tag ()
  "toggle deleted tag for thread"
  (interactive)
  (if (member "deleted" (notmuch-search-get-tags))
	  (notmuch-search-tag '("-deleted"))
	(notmuch-search-tag '("+deleted" "-inbox" "-unread"))))

(defun user:notmuch-toggle-archive-tag ()
  "toggle archive tag for thread"
  (interactive)
  (if (member "archive" (notmuch-search-get-tags))
	  (notmuch-search-tag '("-archive"))
	(notmuch-search-tag '("+archive" "-inbox" "-unread"))))

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
 notmuch-fcc-dirs '(("contact@zell.io" . "zell.io/sent")
					(".*" . "zell.io/sent"))
 notmuch-show-indent-messages-width 4
 notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
						  (:name "unread" :query "tag:unread" :key "u")
						  (:name "flagged" :query "tag:flagged" :key "f")
						  (:name "sent" :query "tag:sent" :key "t")
						  (:name "drafts" :query "tag:draft" :key "d")
						  (:name "all mail" :query "*" :key "a"))

 sendmail-program "/usr/bin/msmtp"

 user-full-name "Zachary Elliott"
 user-mail-address "contact@zell.io")

(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

(define-key notmuch-search-mode-map "g" 'notmuch-poll-and-refresh-this-buffer)
(define-key notmuch-hello-mode-map "g" 'notmuch-poll-and-refresh-this-buffer)

(define-key notmuch-search-mode-map "d" 'user:notmuch-toggle-delete-tag)
(define-key notmuch-show-mode-map "d" 'user:notmuch-toggle-delete-tag)

(define-key notmuch-search-mode-map "a" 'user:notmuch-toggle-archive-tag)
(define-key notmuch-show-mode-map "a" 'user:notmuch-toggle-archive-tag)

;; end of notmuch.el
