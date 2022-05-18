;;; config/10_bindings.el --- key bindings

;; Copyright (C) 2012-2020 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(use-package general
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

   ;; Auto-kill current buffer, don't prompt
   "C-x k" 'user/kill-current-buffer

   ;; Let's make indenting `smarter'
   "TAB" 'user/smart-indent)

  (general-define-key
   :keymaps 'ctl-x-map
   "2" 'user/split-window-below-and-switch
   "3" 'user/split-window-right-and-switch
   "O" 'user/other-window-reverse)

  (general-unbind
   "C-x f"))

;;; config/10_bindings.el ends here
