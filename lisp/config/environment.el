;;; environment.el --- Load shell environment cache -*- lexical-binding: t; coding: utf-8-unix; -*-

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


(use-package envmon
  :functions envmon-mode-enable
  :custom
  (envmon-mode-lighter nil)
  (envmon-shell-executable 'guess)
  (envmon-environment-allow-list 'all)
  (envmon-environment-deny-list
   '("DIRSTACKSIZE" "DISPLAY" "EDITOR" "GID" "GPG_TTY" "GROUP" "HISTFILE"
     "HISTSIZE" "HOME" "IFS" "LESS" "LESSHISTFILE" "LESSOPEN" "OLDPWD"
     "PROMPT" "PWD" "SHLVL" "TERM" "TERMINAL" "UID" "USER"))
  (envmon-async-processing-enabled nil)

  :config
  (envmon-mode-enable))

(provide 'config/environment)

;;; environment.el ends here
