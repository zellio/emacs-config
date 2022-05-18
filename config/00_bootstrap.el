;;; -*- lexical-binding: t -*-

;;; config/emacs/00_bootstrap.el --- set up initial config environment

;; Copyright (C) 2012-2022 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prep emacs for further configuration

;;; Code:


;;; Set up package management

(setq
 ;;; Fix bug
 gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"

 ;; This is an ugly hack becuase they won't leave my init file alone.
 package--init-file-ensured t

 ;; We will run '(package-initialize) in a second
 package-enable-at-startup nil

 package-user-dir (expand-file-name "vendor" user-emacs-directory)
 package-archives '(("org" . "https://orgmode.org/elpa/")
                    ("gnu" . "https://elpa.gnu.org/packages/")
                    ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package-ensure
  :config
  (setq use-package-always-ensure t))


;;; No Littering - Keep extra generated files out of ~/.emacs.d

(use-package no-littering
  :config
  (require 'recentf)

  (push 'no-littering-var-directory recentf-exclude)
  (push 'no-littering-etc-directory recentf-exclude)

  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;;; config/emacs/00_bootstrap.elisp ends here
