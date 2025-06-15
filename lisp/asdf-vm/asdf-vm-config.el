;;; asdf-vm-config.el --- ASDF VM porceline for Emacs -*- lexical-binding: t -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config/main/blob/lisp/asdf-vm-config
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

(defvar asdf-vm-config-file-environment-variable "ASDF_CONFIG_FILE"
  "Path to the .asdfrc configuration file.")

(defcustom asdf-vm-config-file (or (getenv asdf-vm-config-file-environment-variable)
                                   (expand-file-name ".asdfrc" (getenv "HOME")))
  "Path to the .asdfrc configuration file."
  :type 'string
  :group 'asdf-vm)

(defvar asdf-vm-default-tool-versions-filename-environment-variable "ASDF_DEFAULT_TOOL_VERSIONS_FILENAME"
  "The filename of the file storing the tool names and versions.")

(defcustom asdf-vm-default-tool-versions-filename
  (or (getenv asdf-vm-default-tool-versions-filename-environment-variable) ".tool-versions")
  "The filename of the file storing the tool names and versions."
  :type 'string
  :group 'asdf-vm)

(defvar asdf-vm-dir-environment-variable "ASDF_DIR"
  "The location of asdf-vm core scripts.")

(defcustom asdf-vm-dir (or (getenv asdf-vm-dir-environment-variable)
                           (expand-file-name ".asdf-vm" "~"))
  "The location of asdf-vm core scripts."
  :type 'string
  :group 'asdf-vm)

(defvar asdf-vm-data-dir-environment-variable "ASDF_DATA_DIR"
  "The location where asdf-vm will install plugins, shims and tool versions.")

(defcustom asdf-vm-data-dir (or (getenv asdf-vm-data-dir-environment-variable)
                                asdf-vm-dir)
  "The location where asdf-vm will install plugins, shims and tool versions."
  :type 'string
  :group 'asdf-vm)

(defvar asdf-vm-concurrency-environment-variable "ASDF_CONCURRENCY"
  "Number of cores to use when compiling the source code.")

(defcustom asdf-vm-concurrency (or (getenv asdf-vm-concurrency-environment-variable)
                                   "auto")
  "Number of cores to use when compiling the source code."
  :type 'string
  :group 'asdf-vm)

(defvar asdf-vm-force-prepend-environment-variable "ASDF_FORCE_PREPEND"
  "Prepend the asdf-vm shims and path directories to the front of PATH.")

(defcustom asdf-vm-force-prepend (getenv asdf-vm-force-prepend-environment-variable)
  "Prepend the asdf-vm shims and path directories to the front of PATH."
  :type 'string
  :group 'asdf-vm)

(provide 'asdf-vm-config)

;;; asdf-vm-config.el ends here
