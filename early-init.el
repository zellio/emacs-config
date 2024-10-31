;;; early-init.el --- Emacs initialization -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Copyright (C) 2012-2024 Zachary Elliott

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott
;; Version: 0.7.0
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

(setq
 gc-cons-threshold 67108864
 byte-compile-warnings '(not obsolete)
 warning-suppress-log-types '((comp) (bytecomp))
 native-comp-async-report-warnings-errors 'silent
 inhibit-startup-echo-area-message (user-login-name)
 inhibit-startup-screen t
 frame-resize-pixelwise t
 initial-frame-alist '((width . 80)
                       (height . 32)
                       (vertical-scroll-bars . nil)
                       (horizontal-scroll-bars . nil)
                       (fullscreen . nil)
                       (tool-bar-lines . 0)
                       (menu-bar-lines . 0))
 package-enable-at-startup nil
 server-socket-dir "~/.cache/run/emacs")

(when (and (native-comp-available-p) (fboundp 'startup-redirect-eln-cache))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

(provide 'early-init)

;;; early-init.el ends here
