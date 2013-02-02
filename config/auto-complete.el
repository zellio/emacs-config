
;;; auto-complete.el --- auto-complete-mode Configuration

;; Copyright (C) 2012,2013 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(make-directory "~/.emacs.d/ac-data/" t)

(vendor 'popup)
(vendor 'fuzzy)

(require 'auto-complete-config)

(setq ac-comphist-file  "~/.emacs.d/ac-data/ac-comphist.dat")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-data/dict")

(ac-config-default)

;; end of auto-complete.el
