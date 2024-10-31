;;; manner-line-face.el --- Mode line management library -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config
;; Keywords: mode-line

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

;; Personalized mode line

;;; Code:

(defgroup manner-line-face nil
  "Manner line faces customization group."
  :group 'manner-line)

(defface manner-line-default
  '((t (:inherit default)))
  "Manner line default face."
  :group 'manner-line-face)

(defface manner-line-name
  '((t (:inherit bold)))
  "Manner line major-mode name face."
  :group 'manner-line-face)

(defface manner-line-info
  '((t (:inherit info)))
  "Manner line info message face."
  :group 'manner-line-face)

(defface manner-line-warning
  '((t (:inherit warning)))
  "Manner line warning message face."
  :group 'manner-line-face)

(defface manner-line-error
  '((t (:inherit error)))
  "Manner line error message face."
  :group 'manner-line-face)

(defface manner-line-success
  '((t (:inherit success)))
  "Manner line success message face."
  :group 'manner-line-face)

(defface manner-line-failure
  '((t (:inherit error)))
  "Manner line failure message face."
  :group 'manner-line-face)

(defface manner-line-unimportant
  '((t (:inherit shadow)))
  "Manner line unimportant message face."
  :group 'manner-line-face)

(defface manner-line-important
  '((t (:inherit mode-line-highlight)))
  "Manner line important message face."
  :group 'manner-line-face)

(defface manner-line-keyword
  '((t (:inherit font-lock-keyword-face)))
  "Manner line keyword face."
  :group 'manner-line-face)

(defface manner-line-constant
  '((t (:inherit font-lock-constant-face)))
  "Manner line constant face."
  :group 'manner-line-face)

(provide 'manner-line-face)

;;; manner-line-face.el ends here
