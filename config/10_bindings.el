;;; 15_bindings.el --- key bindings -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(use-package general
  :ensure t

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
   "C-'" 'user/match-paren

   ;; set goto-line to just M-g
   "M-g" 'goto-line

   ;; Let's make indenting `smarter'
   "TAB" 'user/smart-indent)

  (general-define-key
   :keymaps 'ctl-x-map
   "2" 'user/split-window-below-and-switch
   "3" 'user/split-window-right-and-switch
   "O" 'user/other-window-reverse
   "k" 'user/kill-current-buffer)

  (general-define-key
   :keymaps 'mode-specific-map
   "l" 'user/snake-case-region
   "u" 'user/upper-camel-case-region)

  (general-unbind
   "C-x f"))

;;; 15_bindings.el ends here
