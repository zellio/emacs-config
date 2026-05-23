;;; progmodes.el --- site-lisp prog mode configuration -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Copyright (C) 2012-2025 Zachary Elliott

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>>
;; Version: 0.8.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'config/emacs)
  (require 'rx)
  (declare-function ansi-color-apply-on-region "ansi-color")
  (declare-function eglot-format-buffer "eglot")
  (declare-function user/setq-local-hook "config/emacs"))

(use-package c-ts-mode
  :preface
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

  :hook (c-ts-mode . eglot-ensure)
  :custom
  (c-ts-mode-indent-offset user/indent-width)
  (c-ts-mode-indent-style 'linux)
  (c-ts-mode-emacs-sources-support nil))

(use-package cc-vars
  :custom (c-require-final-newline nil))

(use-package cmake-ts-mode
  :custom (cmake-ts-mode-indent-offset user/indent-width))

(use-package compile
  :preface
  (defun user/ansi-color-compilation-filter ()
    "Correctly filter ansi colors in compilation buffer."
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region compilation-filter-start (point))))

  :hook (compilation-filter . user/ansi-color-compilation-filter))

(use-package eglot
  :after flycheck yasnippet
  :preface
  (defun user/safe-eglot-format-buffer ()
    ""
    (when (eglot-managed-p)
      (eglot-format-buffer)))

  (defmacro user/add-eglot-workspace-config (server conf-plist)
    ""
    (let ((conf-plist-gensym (gensym "conf-plist")))
      `(with-eval-after-load 'eglot
         (let ((,conf-plist-gensym (quote ,conf-plist)))
           (setq-default
            eglot-workspace-configuration
            (plist-put eglot-workspace-configuration ,server ,conf-plist-gensym))))))

  :commands eglot-managed-p user/add-eglot-workspace-config
  :hook (before-save . user/safe-eglot-format-buffer)
  :custom
  (eglot-connect-timeout 60)
  (eglot-sync-connect 3)
  (eglot-autoshutdown t)
  (eglot-ignored-server-capabilities nil))

(use-package etags
  :custom (tags-apropos-verbose t))

(use-package gdb-mi
  :custom (gdb-debug-log-max 5120))

(use-package go-ts-mode
  :mode ((rx ".go" line-end) . go-ts-mode)
  :hook (go-ts-mode . eglot-ensure)
  :custom (go-ts-mode-indent-offset user/indent-width)
  :config
  (user/add-eglot-workspace-config :gopls
    (:usePlaceholders t)))

(use-package java-ts-mode
  :custom (java-ts-mode-indent-offset user/indent-width))

(use-package js
  :hook (js-ts-mode . eglot-ensure)
  :custom
  (js-indent-level 2)
  (js-indent-first-init 'dynamic))

(use-package json-ts-mode
  :mode ((rx "." (or (sequence "js" (zero-or-one (in ?m ?x))) "har") line-end) . js-ts-mode)
  :custom (json-ts-mode-indent-offset 2))

(use-package python
  :commands user/eglot-python-server
  :mode ((rx ".py" (zero-or-one (or ?i ?w)) line-end) . python-ts-mode)
  :hook (python-ts-mode . eglot-ensure)
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  (python-shell-interpreter-args "-m IPython"))

(use-package proto-ts-mode
  :mode ((rx ".proto" line-end) . proto-ts-mode))

(use-package rust-ts-mode
  :preface
  (defvar user/eglot-rust-server
    '((rust-ts-mode rust-mode) .
      ("rust-analyzer"
       :initializationOptions (:check (:command "clippy")))))

  :mode ((rx ".rs" line-end) . rust-ts-mode)
  :hook (rust-ts-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs user/eglot-rust-server)))

(use-package sh-script
  :preface
  (user/defun-setq-local-hook user/enable-local-indent-tabs-mode
   indent-tabs-mode t)

  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

  :hook
  (sh-mode . user/enable-local-indent-tabs-mode)
  (bash-ts-mode . user/enable-local-indent-tabs-mode))

(use-package typescript-ts-mode
  :mode
  ((rx ".ts" line-end) . typescript-ts-mode)
  ((rx ".tsx" line-end) . tsx-ts-mode)

  :hook
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)

  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package xref
  :after consult
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(provide 'config/progmodes)

;;; progmodes.el ends here
