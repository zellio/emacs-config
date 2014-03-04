
;;; package.el --- Setting marmalade extentions to ELPA

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Not sure what the point of this file is, or why it's still here. I'm sure
;; I'll find a use.

;;; Code:

(require 'package)

(setq package-user-dir (concat user-emacs-directory "/data/elpa"))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; end of package.el
