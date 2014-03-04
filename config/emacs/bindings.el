
;;; bindings.el --- Binding configuration

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

;; Window manipulation
;;   TODO: Find better bindings for this so I can use it in TTY mode
(global-set-key (kbd "C->") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<") 'shrink-window-horizontally)
(global-set-key (kbd "C-.") 'enlarge-window)
(global-set-key (kbd "C-,") 'shrink-window)

;; Keyboard macros
;(global-set-key [(shift f4)] 'kmacro-start-macro-or-insert-counter)
;; (global-set-key [(f4)]    'kmacro-end-or-call-macro)  ;; already defined

;; Refresh-like
(global-set-key (kbd "<f5>")   'revert-buffer)
(global-set-key (kbd "C-<f5>") 'revert-all-buffers)

;; Indenting and alignment
(global-set-key (kbd "<f8>")   'indent-region)
(global-set-key (kbd "C-<f8>") 'align)
(global-set-key (kbd "S-<f8>") 'align-current)
(global-set-key (kbd "M-<f8>") 'align-regexp)

;; Find matching parens
(global-set-key (kbd "C-'") 'match-paren)

;; set goto-line to just M-g
(global-set-key (kbd "M-g") 'goto-line)

;; Auto-kill current buffer, don't prompt
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Don't use this anyway
(global-unset-key (kbd "C-x f"))

;; end of bindings.el
