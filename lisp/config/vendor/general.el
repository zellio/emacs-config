;;; general.el --- User configured command bindings -*- lexical-binding: t; coding: utf-8-unix; -*-

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

(use-package general
  :preface
  (defun user/kill-current-buffer ()
    "Kill the current buffer without prompting the user."
    (interactive)
    (kill-buffer (current-buffer)))

  (defun user/goto-matching-parenthesis (_)
    "Go to the matching paren if on a paren"
    (interactive "p")
    (cond ((looking-at (rx (syntax open-parenthesis))) (forward-list 1) (backward-char 1))
          ((looking-at (rx (syntax close-parenthesis))) (forward-char 1) (backward-list 1))))

  (defun user/other-window-reverse (count &optional all-frames)
    ""
    (interactive "p")
    (let ((count (* -1 count)))
      (other-window count all-frames)))

  (defun user/revert-all-buffers ()
    "Replace all buffers with their visited file contents on disk."
    (interactive)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and
               (buffer-file-name)
               (file-exists-p (buffer-file-name))
               (buffer-modified-p))
          (revert-buffer t t t)
          (message "Reverted buffer %s" buffer)))))

  (defun user/split-window-below-and-switch ()
    "Split window below and switch to it."
    (interactive)
    (split-window-below)
    (other-window 1))

  (defun user/split-window-right-and-switch ()
    "Split window to the right and switch to it."
    (interactive)
    (split-window-right)
    (other-window 1))

  :functions general-define-key general-unbind
  :config
  (general-define-key
   ;;; Navidation
   "M-n" 'forward-paragraph
   "M-p" 'backward-paragraph

   ;;; Refresh buffers
   "<f5>" 'revert-buffer
   "S-<f5>" 'user/revert-all-buffers

   ;;; Indenting and alignment
   "<f8>" 'indent-region
   "C-<f8>" 'align
   "S-<f8>" 'align-current
   "M-<f8>" 'align-regexp

   ;; Find matching parens
   "C-'" 'user/goto-matching-parenthesis

   ;; set goto-line to just M-g
   "M-g" 'goto-line)

  (general-define-key
   :keymaps 'ctl-x-map
   "2" 'user/split-window-below-and-switch
   "3" 'user/split-window-right-and-switch
   "O" 'user/other-window-reverse
   "k" 'user/kill-current-buffer)

  (general-unbind
    "C-x f"))

(provide 'config/vendor/general)

;;; general.el ends here
