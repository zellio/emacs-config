;;; config/pkg/ido.el --- interactive do configuration

;; Copyright (C) 2012-2017 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'ido)

(ido-mode t)

(setq
 ido-enable-flex-matching t
 ido-enable-last-directory-history nil
 ido-everywhere t
 ido-use-filename-at-point 'guess)

;;; config/pkg/ido.el ends here
