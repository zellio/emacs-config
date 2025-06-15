;;; asdf-vm.el --- ASDF VM porceline for Emacs -*- lexical-binding: t -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config/main/blob/lisp/asdf-vm
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

(defgroup asdf-vm nil
  "Configuration group for ASDF-VM."
  :prefix "asdf-vm-"
  :group 'tools)

(require 'asdf-vm-error)
(require 'asdf-vm-config)
(require 'asdf-vm-process)
(require 'asdf-vm-util)
(require 'asdf-vm-core)
(require 'asdf-vm-plugin)
(require 'asdf-vm-mode)

(provide 'asdf-vm)

;;; asdf-vm.el ends here
