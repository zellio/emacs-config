;;; progmodes.el --- vendored progmode package configuration -*- lexical-binding: t; coding: utf-8-unix; -*-

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
  (require 'rx))

(use-package bats-mode)

(use-package bison-mode)

(use-package dockerfile-mode
  :init
  (put 'docker-image-name 'safe-local-variable #'stringp)
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)

  :custom
  (dockerfile-build-pull t)
  (dockerfile-build-args '("--rm"))
  (dockerfile-build-progress "plain")
  (dockerfile-use-buildkit t)
  (dockerfile-enable-auto-indent nil))

(use-package gotmpl-ts-mode
  :ensure
  (:host github
   :repo "zellio/tree-sitter-gotmpl"
   :files (:defaults "extras/gotmpl-ts-mode/*.el")))

(use-package proto-ts-mode
  :ensure nil
  :mode ((rx ".proto" line-end) . proto-ts-mode))

(use-package plantuml-mode
  :mode ((rx ".puml" line-end) . plantuml-mode)
  :custom
  (plantuml-executable-path (executable-find "plantuml"))
  (plantuml-default-exec-mode 'executable)
  (plantuml-indent-level 8))

(use-package scad-mode)

(use-package terraform-mode
  :hook (terraform-mode . eglot-ensure)
  :custom
  (terraform-indent-level 2)
  (terraform-format-on-save-mode t)

  :config
  (with-eval-after-load 'eglot
    (defvar eglot-server-programs)
    (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))))

(provide 'config/vendor/progmodes)

;;; progmodes.el ends here
