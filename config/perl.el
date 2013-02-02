
;;; perl.el --- cperl-mode Configuration

;; Copyright (C) 2012,2013 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;; TODO: Fix font-lock colours, they are still bad

;;; Code:


(defalias 'perl-mode 'cperl-mode)

(setq
 cperl-highlight-variables-indiscriminately t
 cperl-invalid-face nil)

(custom-set-faces
 '(cperl-array-face          ((t (:foreground "DarkGreen" :background "Clear"))))
 '(cperl-hash-face           ((t (:foreground "DodgerBlue4" :background "Clear"))))
 '(cperl-nonoverridable-face ((t (:foreground "Black" :background "Clear")))))

;; end of perl.el
