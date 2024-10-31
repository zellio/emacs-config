;;; manner-line-flycheck.el --- Manner line flycheck segment -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.0") (flycheck  "35.0"))
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

(require 'flycheck)
(require 'manner-line-face)

(eval-when-compile
  (declare-function flycheck-count-errors "flycheck")
  (declare-function flycheck-first-error "flycheck")
  (declare-function flycheck-list-errors "flycheck"))

;;; Customization Group

(defgroup manner-line-flycheck nil
  "Customization group for `manner-line-flycheck'."
  :group 'manner-line)

(defvar manner-line-flycheck-hook-alist
  '((manner-line-flycheck-segment-update
     flycheck-mode-hook
     flycheck-status-changed-functions))
  "Flycheck `manner-line' feature hooks.")

(defvar manner-line-flycheck-masked-symbols nil
  "Flycheck `manner-line' feature values.")

(defvar-local manner-line-flycheck-segment
    `("FlyC[" (:propertize "-" face manner-line-unimportant) "]")
  "Buffer local cache for `manner-line-mode' flycheck segment.")

(put 'manner-line-flycheck-segment 'risky-local-variable t)

(defcustom manner-line-flycheck-symbol-alist
  '((not-checked . "=")
    (no-checker . "-")
    (running . "R")
    (errored . "!")
    (interrupted . "*")
    (suspicious .  "?")
    (finished . "f"))
  "Symbol lookup table for information display."
  :group 'manner-line-flycheck
  :type '(alist :key-type symbol :value-type string))

(defcustom manner-line-flycheck-face-alist
  '((not-checked . manner-line-default)
    (no-checker . manner-line-unimportant)
    (running . manner-line-default)
    (errored . manner-line-error)
    (interrupted . manner-line-default)
    (suspicious . manner-line-warning)
    (finished . manner-line-info))
  "Symbol lookup table for information display."
  :group 'manner-line-flycheck
  :type '(alist :key-type symbol :value-type symbol))

(defun manner-line-flycheck-segment-indicator (&optional status)
  "Internal update function for flycheck STATUS."
  (if (eq status 'finished)
      (let ((default #("0" 0 1 (face manner-line-unimportant))))
        (let-alist (flycheck-count-errors flycheck-current-errors)
          (list
           (if .error
               `(:propertize
                 ,(format "%d" .error)
                 face manner-line-error
                 help-echo "Goto first flycheck error"
                 mouse-face manner-line-important
                 local-map ,(make-mode-line-mouse-map 'mouse-1 #'flycheck-first-error))
             default)
           " "
           (if .warning
               `(:propertize
                 ,(format "%d" .warning)
                 face manner-line-warning
                 help-echo "Goto first flycheck error"
                 mouse-face manner-line-important
                 local-map ,(make-mode-line-mouse-map 'mouse-1 #'flycheck-first-error))
             default)
           " "
           (if .info
               `(:propertize ,(format "%d" .info) face manner-line-info)
             default))))
    (let* ((display-text (alist-get status manner-line-flycheck-symbol-alist (symbol-name status))))
      (when-let* ((face (alist-get status manner-line-flycheck-face-alist)))
        (add-face-text-property 0 (length display-text) face nil display-text))
      display-text)))

;;;###autoload
(defun manner-line-flycheck-segment-update (&optional status)
  "Update `manner-line-mode' flycheck segment based on current STATUS."
  (let* ((status (or status flycheck-last-status-change))
         (local-map (make-mode-line-mouse-map 'mouse-1 #'flycheck-list-errors))
         (help-message
          (pcase 'status
            ('not-checked "The current buffer was not checked.")
            ('no-checker "Automatic syntax checker selection did not find a suitable syntax checker.")
            ('running "A syntax check is now running in the current buffer.")
            ('errored "The current syntax check has errored.")
            ('finished "The current syntax check was finished normally.")
            ('interrupted "The current syntax check was interrupted.")
            ('suspicious "The last syntax check had a suspicious result."))))
    (setq
     manner-line-flycheck-segment
     (list
      (propertize "FlyC" 'display '(raise 0.0) 'local-map local-map 'help-echo help-message)
      "["
      (manner-line-flycheck-segment-indicator status)
      "]"))))

(provide 'manner-line-flycheck)

;;; manner-line-flycheck.el ends here
