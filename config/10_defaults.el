;;; config/10_defaults.el --- default configurations

;; Copyright (C) 2012-2019 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq-default
 inhibit-startup-message t
 default-tab-width 4
 c-basic-offset 4
 kill-whole-line t
 truncate-partial-width-windows nil
 fill-column 78
 indicate-empty-lines t
 line-number-mode t
 column-number-mode t
 visible-bell t
 truncate-lines t
 require-final-newline t

 ;; tab controls
 indent-tabs-mode nil
 standard-indent 4
 tab-width 4

 ;; utf-8 defaults
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; config/10_global.el ends here
