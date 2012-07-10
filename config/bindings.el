
;;; bindings.el --- Binding configuration

;; Copyright (C) 2012 Zachary Elliott
;;
;; Authors: Zachary Elliott <ZacharyElliott1@gmail.com>
;; URL:
;; Version: 1.0.0
;; Keywords:

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


;; Window manipulation
(global-set-key (kbd "C->") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<") 'shrink-window-horizontally)
(global-set-key (kbd "C-.") 'enlarge-window)
(global-set-key (kbd "C-,") 'shrink-window)

;; Keyboard macros
;(global-set-key [(shift f4)] 'kmacro-start-macro-or-insert-counter)
;; (global-set-key [(f4)]    'kmacro-end-or-call-macro)  ;; already defined

;; Refresh-like
(global-set-key (kbd "<f5>")   'revert-buffer)
(global-set-key (kbd "C-<f5>") 'revbufs)

;; Indenting and alignment
(global-set-key (kbd "<f8>")   'indent-region)
(global-set-key (kbd "C-<f8>") 'align)
(global-set-key (kbd "S-<f8>") 'align-current)
(global-set-key (kbd "M-<f8>") 'align-regexp)

;; Find matching parens
(global-set-key (kbd "C-'") 'match-paren)

;; set goto-line to just M-g
(global-set-key (kbd "M-g") 'goto-line)
