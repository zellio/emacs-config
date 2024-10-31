;;; url.el --- Emacs url configuration -*- lexical-binding: t; coding: utf-8-unix; -*-

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

(use-package url-history
  :after no-litering
  :custom
  (url-history-track t)
  (url-history-file (no-littering-expand-var-file-name "url/history")))

(use-package url-vars
  :custom
  (url-automatic-caching t)
  (url-privacy-level '(email os emacs))
  (url-max-redirections 16))

(provide 'config/url)

;;; url.el ends here
