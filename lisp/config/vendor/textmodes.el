;;; textmodes.el --- vendored textmode package configuration -*- lexical-binding: t -*-

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

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :general
  ([remap ispell-word] 'jinx-correct
   "C-M-$" 'jinx-languages)

  :config
  (with-eval-after-load 'vertico-multiform
    (add-to-list
     'vertico-multiform-categories
     '(jinx grid (vertico-grid-annotate . 20) (vertico-count . 4)))))

(provide 'config/vendor/textmodes)

;;; textmodes.el ends here
