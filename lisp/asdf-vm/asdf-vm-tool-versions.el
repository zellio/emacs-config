;;; asdf-vm-tool-versions.el --- ASDF VM porceline for Emacs -*- lexical-binding: t -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config/main/blob/lisp/asdf-vm-tool-versions
;; Keywords: languages asdf

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

(require 'asdf-vm-plugin)
(require 'asdf-vm-util)

(defgroup asdf-vm-tool-versions nil
  "Configuration group for ASDF-VM tool versions."
  :prefix "asdf-vm-tool-versions-"
  :group 'asdf-vm)

(defun asdf-vm-tool-versions--locate-dominating-file (path)
  "Starting at PATH, look up the directory hierarchy for .tool-versions."
  (let ((tools-version-path (expand-file-name ".tool-versions" path)))
    (if (file-readable-p tools-version-path)
        tools-version-path
      (if-let* ((parent (file-name-parent-directory path)))
          (asdf-vm-tool-versions--locate-dominating-file parent)
        (let* ((global-file (expand-file-name ".tool-versions" "~")))
          (and (file-readable-p global-file) global-file))))))

(defun asdf-vm-tool-versions-locate (&optional path interactive-call)
  "Starting at PATH, look up the directory hierarchy for .tool-versions.

INTERACTIVE-CALL is an internal flag and should not be passed manually."
  (interactive "GParent: \np")
  (and-let* ((path (or path (buffer-file-name (current-buffer)) default-directory))
             (tool-versions-file (asdf-vm-tool-versions--locate-dominating-file path)))
    (when interactive-call
      (asdf-vm-message "Dominating tool-versions file for %s is %s" path tool-versions-file))
    tool-versions-file))

(define-widget 'asdf-vm-tool-versions-file 'lazy
  "Content of an asdf-vm versions file"
  :type `(group
          :tag "ASDF-VM tool-versions file"
          :format "bahh %t\n%v\n"
          (repeat
           :tag "Tools"
           :format "%t: %i %d\n%v\n"
           (group (string :tag "Tool name" :completions ,(asdf-vm-plugin--installed-plugins))
                  (repeat :tag "Versions" :format "%t: %i %d\n%v" (string :tag "Version"))))))

(provide 'asdf-vm-tool-versions)

;;; asdf-vm-tool-versions.el ends here
