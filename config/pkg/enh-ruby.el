;;; config/pkg/enh-ruby.el --- enh-ruby configuration

;; Copyright (C) 2012-2017 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

;; end of enh-ruby.el

(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

(setq
 enh-ruby-bounce-deep-indent t
 enh-ruby-hanging-indent-level 0)

;;; config/pkg/enh-ruby.el ends here
