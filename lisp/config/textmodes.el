;;; textmodes.el --- site-lisp text mode configuration -*- lexical-binding: t; coding: utf-8-unix; -*-

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
  (require 'config/emacs))

(use-package toml-ts-mode
  :custom (toml-ts-mode-indent-offset user/indent-width))

(use-package yaml-ts-mode
  :preface
  (user/defun-setq-local-hook user/set-yaml-ts-mode-tab-width
   tab-width 2)

  :hook
  (yaml-ts-mode . user/set-yaml-ts-mode-tab-width))

(provide 'config/textmodes)

;;; textmodes.el ends here
