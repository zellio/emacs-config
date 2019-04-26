;;; config/10_bindings.el --- key bindings

;; Copyright (C) 2012-2019 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

;; Window manipulation
;;   TODO: Find better bindings for this so I can use it in TTY mode
;; (global-set-key (kbd "C->") 'enlarge-window-horizontally)
;; (global-set-key (kbd "C-<") 'shrink-window-horizontally)
;; (global-set-key (kbd "C-.") 'enlarge-window)
;; (global-set-key (kbd "C-,") 'shrink-window)

;; Navigation
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Refresh-like
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "S-<f5>") 'user/revert-all-buffers)

;; Indenting and alignment
(global-set-key (kbd "<f8>") 'indent-region)
(global-set-key (kbd "C-<f8>") 'align)
(global-set-key (kbd "S-<f8>") 'align-current)
(global-set-key (kbd "M-<f8>") 'align-regexp)

;; Find matching parens
(global-set-key (kbd "C-'") 'user/match-paren)

;; set goto-line to just M-g
(global-set-key (kbd "M-g") 'goto-line)

;; Auto-kill current buffer, don't prompt
(global-set-key (kbd "C-x k") 'user/kill-current-buffer)

;; Let's make indenting `smarter'
(global-set-key (kbd "TAB") 'user/smart-indent)

;; Don't use this anyway
(global-unset-key (kbd "C-x f"))

;;; config/10_bindings.el ends here
