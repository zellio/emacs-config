
;;; tramp.el --- TRAMP configuration

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'tramp)

(setq
 tramp-default-method "ssh"
 tramp-persistency-file-name
 (expand-file-name "tramp-connection-history" user:emacs-data-directory))

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)



;; end of tramp.el
