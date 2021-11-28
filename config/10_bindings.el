;;; config/10_bindings.el --- key bindings

;; Copyright (C) 2012-2020 Zachary Elliott
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
(define-key global-map (kbd "M-n") 'forward-paragraph)
(define-key global-map (kbd "M-p") 'backward-paragraph)

;; Refresh-like
(define-key global-map (kbd "<f5>") 'revert-buffer)
(define-key global-map (kbd "S-<f5>") 'user/revert-all-buffers)

;; Indenting and alignment
(define-key global-map (kbd "<f8>") 'indent-region)
(define-key global-map (kbd "C-<f8>") 'align)
(define-key global-map (kbd "S-<f8>") 'align-current)
(define-key global-map (kbd "M-<f8>") 'align-regexp)

;; Find matching parens
(define-key global-map (kbd "C-'") 'user/match-paren)

;; set goto-line to just M-g
(define-key global-map (kbd "M-g") 'goto-line)

;; Auto-kill current buffer, don't prompt
(define-key global-map (kbd "C-x k") 'user/kill-current-buffer)

;; Let's make indenting `smarter'
(define-key global-map (kbd "TAB") 'user/smart-indent)

;; Don't use this anyway
(global-unset-key (kbd "C-x f"))

;;; config/10_bindings.el ends here
