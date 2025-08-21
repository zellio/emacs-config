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

(use-package auctex
  :ensure (:repo "git://git.savannah.gnu.org/auctex.git"
           :branch "main"
           :pre-build (("make" "elpa"))
           :build (:not elpaca--compile-info)
           :files ("*.el" "doc/*.info" "etc" "images" "latex" "style")
           :version (lambda (_) (require 'auctex) AUCTeX-version)))

(use-package bib-cite
  :ensure nil
  :after auctex
  :custom (bib-novice nil))

(use-package latex
  :ensure nil
  :after auctex
  :custom
  (LaTeX-default-options "a4paper")
  (LaTeX-insert-into-comments t)
  (LaTeX-default-environment "itemize*")
  (LaTeX-syntactic-comments nil)
  (LaTeX-math-list '((?B "Beta" "Greek Uppercase" #x0392)
                     (?H "Eta" "Greek Uppercase" #x0397)
                     (?\C-u "sum" "Var Symbol" #x2211)
                     (?\C-o "prod" "Var Symbol" #x220F)
                     (?\C-r "sqrt" "Constructs" #x221A)
                     (?v "vec" "Accents" #X20D7))))

(use-package tex
  :ensure nil
  :after auctex
  :custom
  (TeX-engine 'luatex)
  (TeX-master nil)
  (TeX-parse-self t)
  (TeX-complete-word nil)
  (TeX-auto-save t)
  (TeX-auto-untabify t)

  :config
  (setcdr (assoc 'output-pdf TeX-view-program-selection) '("PDF Tools")))

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
