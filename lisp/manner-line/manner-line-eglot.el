;;; manner-line-eglot.el --- Mode line management library -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.0") (eglot))
;; Homepage: https://github.com/zellio/emacs-config
;; Keywords: mode-line

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

;; Personalized mode line

;;; Code:

(require 'eglot)
(require 'manner-line-face)

(defvar-local manner-line-eglot-segment
    `(""
      (:propertize
       "Eglot"
       mouse-face manner-line-important
       local-map ,(make-mode-line-mouse-map 'down-mouse-1 #'eglot-menu))
      "["
      (:eval (manner-line-eglot-segment-indicator))
      "]")
  "Eglot `mode-line' segment value.")

(put 'manner-line-eglot-segment 'risky-local-variable t)

(defun manner-line-eglot-segment-indicator (&rest _args)
  "Internal status information for eglot mode-line segement."
  (let* ((current-server (eglot-current-server)) (values))
    (when-let* ((server-info (eglot--server-info current-server))
                (server-name (or (plist-get server-info :name) (jsonrpc-name current-server))))
      (setq values
            (cons
             `(:propertize
               ,server-name
               face manner-line-keyword
               mouse-face manner-line-important
               help-echo "mouse-1: LSP server control menu"
               keymap ,(make-mode-line-mouse-map 'down-mouse-1 #'eglot-server-menu))
             values)))
    (when-let* ((last-error (jsonrpc-last-error current-server)))
      (setq values
            (cons
             `("/"
               (:propertize
                "Err"
                face manner-line-error
                help-echo "mouse-3: Clear status"
                mouse-face manner-line-important
                keymap ,(make-mode-line-mouse-map 'mouse-3 'eglot-clear-status)))
             values)))
    (when-let* ((pending (jsonrpc-continuation-count current-server)) (_ (> pending 0)))
      (setq values
            (cons
             `("/"
               (:propertize
                ,(format "%d" pending)
                face manner-line-warning
                help-echo "mouse-3: Forget pending continuations"
                mouse-face manner-line-important
                keymap ,(make-mode-line-mouse-map 'mouse-3 'eglot-forget-pending-continuations)))
             values)))
    (cl-loop
     for progress-reporter hash-values of (eglot--progress-reporters current-server)
     when (eq (car progress-reporter) 'eglot--mode-line-reporter)
     do (setq values
              (cons `("/"
                      (:propertize
                       ,(format "%s%%" (or (nth 4 progress-reporter) "?"))
                       help-echo ,(apply #'format "(%s) %s %s" (take 3 progress-reporter))))
                    values)))
    (reverse values)))

(provide 'manner-line-eglot)

;;; manner-line-eglot.el ends here
