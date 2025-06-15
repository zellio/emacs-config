;;; envmon-table.el --- Tabulated list display for environment variables -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.2.0
;; Package-Requires: ((emacse "30.0") (cl-macs))
;; Homepage: http://github.com/zellio/emacs-config/blob/main/lisp/envmon
;; Keywords: environment, linux, shell

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

(require 'cl-macs)
(require 'envmon)

(defgroup envmon-table nil
  "Configuration group for `envmon-table'."
  :prefix "envmon-table-"
  :group 'envmon)

(defcustom envmon-table-buffer-name "*envmon-table*"
  "`envmon-table' display buffer name."
  :group 'envmon-table
  :type 'string)

(defcustom envmon-table-padding-width 2
  "`tabulated-list-padding' value for `envmon-table'."
  :group 'envmon-table
  :type 'number)

(defcustom envmon-table-name-column-width 32
  "`tabulated-list-format' column width for `envmon-table'."
  :group 'envmon-table
  :type 'number)

(defvar envmon-table--tabulated-list-format
  (vector (list "Name" envmon-table-name-column-width t)
          (list "Value" 0 t))
  "`tabulated-list-format' for `envmon-table'.")

(defvar-local envmon-table--tabulated-list-entries
    nil
  "`tabulated-list-entries' for `envmon-table'.")

(defvar-local envmon-table--environment-alist
    nil
  "Internal state var for `envmon-table'.")

(defun envmon-table--tabulated-list-entries (&optional environment-alist)
  "`tabulated-list-entries' for `envmon-table'.

When ENVIRONMENT-ALIST is nil, display the value of
`envmon-table--environment-alist'."
  (cl-labels
      ((escape-value (value)
         (string-replace "\r" "\\r" (string-replace "\n" "\\n" value)))
       (generate-row (environment-cons)
         (pcase-let ((`(,name . ,value) environment-cons))
           (and name (list name (vector name (escape-value (or value ""))))))))
    (let* ((environment-alist (or environment-alist envmon-table--environment-alist)))
      (seq-map #'generate-row environment-alist))))

(define-derived-mode envmon-table-mode tabulated-list-mode "Environment Variables"
  "Major mode for visualing environment variables."
  (setq
   envmon-table--environment-alist (seq-map #'envmon--parse-environment-cons process-environment)
   tabulated-list-use-header-line t
   tabulated-list-format envmon-table--tabulated-list-format
   tabulated-list-padding envmon-table-padding-width
   tabulated-list-entries #'envmon-table--tabulated-list-entries)
  (tabulated-list-init-header))

(defun envmon-list-environment ()
  "Create and switch to `envmon-table' tabulated list."
  (interactive)
  (let* ((buffer (get-buffer-create envmon-table-buffer-name)) (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (envmon-table-mode)
      (tabulated-list-print)
      (pop-to-buffer buffer))))

(provide 'envmon-table)

;;; envmon-table.el ends here
