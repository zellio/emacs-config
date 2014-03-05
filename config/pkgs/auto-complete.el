
;;; auto-complete.el --- auto-complete-mode Configuration

;; Copyright (C) 2012,2013 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq
 ac-data-directory (expand-file-name "auto-complete" user-emacs-data-directory))

(make-directory ac-data-directory t)

(require 'auto-complete-config)

(setq ac-comphist-file (expand-file-name "comphist.dat" ac-data-directory))

(add-to-list
 'ac-dictionary-directories (expand-file-name "dict" ac-data-directory))

(ac-config-default)

;; end of auto-complete.el
