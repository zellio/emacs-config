;;; manner-line-version-control.el --- Mode line management library -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.0") (nerd-icons))
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

(require 'manner-line-face)
(require 'nerd-icons)

(defgroup manner-line-version-control nil
  "Customization group for `manner-line-version-control'."
  :group 'manner-line)

(defvar manner-line-version-control-hook-alist
  '((manner-line-version-control-segment-update
     find-file-hook
     after-save-hook))
  "Projectile `manner-line' feature hooks.")

(defvar manner-line-version-control-masked-symbols nil
  "Projectile `manner-line' feature values.")

(defvar-local manner-line-version-control-segment nil
  "Local cache value for `manner-line-mode' vc state segment.")

(put 'manner-line-version-control-segment 'risky-local-variable t)

(defcustom manner-line-version-control-state-symbol-alist
  '((added . "+")
    (edited . "!")
    (needs-update . "U")
    (needs-merge . "M")
    (removed . "R")
    (conflict . "C")
    (missing . "S")
    (unregistered . "N")
    (up-to-date . "=")
    (ignored . "-"))
  "Symbol lookup table for information display."
  :group 'manner-line-version-control
  :type '(alist :key-type symbol :value-type string))

(defcustom manner-line-version-control-state-face-alist
  '((added . manner-line-success)
    (edited . manner-line-warning)
    (needs-update . manner-line-warning)
    (needs-merge . manner-line-warning)
    (removed . manner-line-error)
    (conflict . manner-line-error)
    (missing . manner-line-error)
    (unregistered . manner-line-error)
    (up-to-date . manner-line-success)
    (ignored . manner-line-success))
  "Symbol lookup table for information display."
  :group 'manner-line-version-control
  :type '(alist :key-type symbol :value-type string))

(defun manner-line-version-control-segment-update ()
  "Update hook for `manner-line-mode' vc state segment."
  (setq
   manner-line-version-control-segment
   (let ((backend
          (and-let*
              ((vc-backend (vc-backend buffer-file-name))
               (vc-mode-line (vc-call-backend vc-backend 'mode-line-string buffer-file-name)))
            (substring-no-properties vc-mode-line (+ 1 (length (symbol-name vc-backend))))))
         (status
          (and-let*
              ((state (vc-state buffer-file-name))
               (display-text
                (alist-get state manner-line-version-control-state-symbol-alist (symbol-name state))))
            (when-let*
                ((face (alist-get state manner-line-version-control-state-face-alist)))
              (add-face-text-property 0 (length display-text) face nil display-text))
            display-text)))
     (list "[" backend (and backend status " ") status "]"))))

(provide 'manner-line-version-control)

;;; manner-line-version-control.el ends here
