;;; init.el --- Base installation file -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Copyright (C) 2012-2024 Zachary Elliott

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott
;; Version: 0.7.0
;; Package-Requires: ((emacs "29.0"))
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

;; Add local lisp directories to load path

(let* ((lisp-directory (expand-file-name "lisp" user-emacs-directory)))
  (dolist (directory (directory-files lisp-directory t (rx line-start (not ?.)) t))
    (when (not (string-suffix-p "/config" directory))
      (push directory load-path)))
  (push lisp-directory load-path))

;; Load user configuration

(let* ((file-name-handler-alist nil))
  (require 'config/bootstrap)
  (require 'config/environment)
  (require 'config/emacs)
  (require 'config/vendor/general)
  (require 'config/textmodes)
  (require 'config/progmodes)
  (customize-set-variable 'use-package-always-ensure t)
  (require 'config/vendor/interface)
  (require 'config/vendor/tools)
  (require 'config/vendor/org)
  (require 'config/vendor/textmodes)
  (require 'config/vendor/progmodes)
  (require 'config/theme)

  ;; Move user configuration file
  (use-package cus-edit
    :custom
    (custom-file (no-littering-expand-etc-file-name "custom.el")))

  ;; Load custom and un-tracked site config
  (dolist (config-file (list custom-file (no-littering-expand-etc-file-name "site-config.el")))
    (when (file-readable-p config-file)
      (load config-file))))

;; Report startup statistics

(message "[init] loaded in %.2f seconds with %d garbage collections."
         (float-time (time-subtract after-init-time before-init-time))
         gcs-done)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'init)

;;; init.el ends here
