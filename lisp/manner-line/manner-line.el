;;; manner-line.el --- Mode line management library -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.0")
;;                    (eglot)
;;                    (flycheck)
;;                    (nerd-icons)
;;                    (project))
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

(defgroup manner-line nil
  "`manner-line' customization group."
  :group 'mode-line
  :prefix "manner-line-")

(define-error 'manner-line-error "[manner-line] Base error" '(error))

(defcustom manner-line-features nil
  "List of enabled `manner-line' features."
  :group 'manner-line
  :type '(list symbol))

(defcustom manner-line-hook-alist nil
  "Alist of user-controled hooks for `manner-line-mode'."
  :group 'manner-line
  :type '(alist :key-type symbol :value-type (list symbol)))

(defcustom manner-line-masked-symbols nil
  "List of user-controled masking values for `manner-line-mode'."
  :group 'manner-line
  :type '(list symbol))

(defcustom manner-line-header " "
  "Prefix string for rendered `mode-line'."
  :group 'manner-line
  :type 'string)

(defcustom manner-line-footer " "
  "Postfix string for rendered `mode-line'."
  :group 'manner-line
  :type 'string
  :safe 'stringp)

(defcustom manner-line-spacer "  "
  "Spacer between segments in rendered `mode-line'."
  :group 'manner-line
  :type 'string
  :safe 'stringp)

(defcustom manner-line-format nil
  "List of segments to render into `mode-line'."
  :group 'manner-line
  :type '(list sexp))

(defvar manner-line--enabled-features nil
  "Internal enabled feature list.")

(defvar manner-line--hooks-alist nil
  "Internal feature hook alist.")

(defvar manner-line--symbols-alist nil
  "Internal feature symbol alist.")

(defvar manner-line-default-hook-alist nil
  "Default `manner-line' feature value.")

(defvar manner-line-default-masked-symbols nil
  "Default `manner-line' feature value.")

(defsubst manner-line-feature-enabled-p (feature)
  "Check if FEATURE is a member of `manner-line-features'."
  (memq feature manner-line--enabled-features))

(define-error
 'manner-line-feature-missing
 "[manner-line] Failed to locate feature"
 '(manner-line-error))

(defun manner-line-feature-value (feature symbol)
  "Attempt to resolve SYMBOL in the FEATURE context."
  (let ((feature-symbol (intern (format "manner-line-%s-%s" feature symbol))))
    (cond ((boundp feature-symbol) (symbol-value feature-symbol))
          ((not (eq feature 'default)) (manner-line-feature-value 'default symbol))
          (t (signal 'manner-line-feature-missing (list symbol))))))

(define-error
 'manner-line-feature-function-missing
 "[manner-line] Failed to locate feature function"
 '(manner-line-error))

(defun manner-line-feature-call (feature function &rest args)
  "Attempt to call FUNCTION from FEATURE with ARGS."
  (let ((func (intern (format "manner-line-%s-%s" feature function))))
    (cond ((functionp func) (apply #'funcall func args))
          ((not (eq feature 'default))
           (apply #'manner-line-feature-call 'default function args))
          (t (signal 'manner-line-feature-function-missing (list function))))))

(defun manner-line-enable-feature-hooks (feature &optional hook-alist)
  "Enable hooks of FEATURE HOOK-ALIST."
  (when-let* ((hook-alist (or hook-alist (manner-line-feature-value feature 'hook-alist))))
    (pcase-dolist (`(,func . ,hooks) hook-alist)
      (dolist (hook hooks)
        (add-hook hook func)))
    (push (cons feature hook-alist) manner-line--hooks-alist)))

(defun manner-line-disable-feature-hooks (feature)
  "Disable hooks of FEATURE."
  (when-let* ((hook-alist (alist-get feature manner-line--hooks-alist)))
    (pcase-dolist (`(,func . ,hooks) hook-alist)
      (dolist (hook hooks)
        (remove-hook hook func)))
    (setq manner-line--hooks-alist (assq-delete-all feature manner-line--hooks-alist))))

(defun manner-line-enable-feature-mask (feature &optional masked-symbols)
  "Enable FEATURE MASKED-SYMBOLS symbol mask."
  (cl-labels ((symbol-cell (symbol)
                (let ((value (if (default-boundp symbol) (default-value symbol)
                               (symbol-value symbol))))
                  (cons symbol value))))
    (when-let* ((masked-symbols (or masked-symbols (manner-line-feature-value feature 'masked-symbols))))
      (push (cons feature (mapcar #'symbol-cell masked-symbols)) manner-line--symbols-alist))))

(defun manner-line-disable-feature-mask (feature)
  "Disable FEATURE MASKED-SYMBOLS symbol mask."
  (when-let* ((masked-symbols (alist-get feature manner-line--symbols-alist)))
    (pcase-dolist (`(,symbol . ,value)  masked-symbols)
      (if (default-boundp symbol) (set-default symbol value)
        (set symbol value)))
    (setq manner-line--symbols-alist (assq-delete-all feature manner-line--symbols-alist))))

;;;###autoload
(defun manner-line-enable-feature (feature)
  "Enable FEATURE hooks and symbols."
  (interactive "SFeature: ")
  (let ((module (intern (format "manner-line-%s" feature))))
    (unless (memq module features) (require module))
    (manner-line-enable-feature-hooks feature)
    (manner-line-enable-feature-mask feature)
    (push feature manner-line--enabled-features)))

;;;###autoload
(defun manner-line-disable-feature (feature)
  "Disalbe FEATURE hooks and symbols."
  (interactive "SFeature: ")
  (let ((module (intern (format "manner-line-%s" feature))))
    (manner-line-disable-feature-mask feature)
    (manner-line-disable-feature-hooks feature)
    (unload-feature module)
    (delq feature manner-line--enabled-features)))

;;; Core Mode Line Segments

(defvar-local manner-line-position-segment
    `(""
      (:propertize "(%l,%c)" ,@mode-line-position--column-line-properties)
      " "
      (:propertize "%o" face manner-line-unimportant))
  "Cursor position `most-line' segment.")

(put 'manner-line-position-segment 'risky-local-variable t)

(defvar-local manner-line-major-mode-segment
    `(""
      (:propertize
       (:eval (substring-no-properties (format-mode-line mode-name)))
       face manner-line-name)))

(put 'manner-line-major-mode-segment  'risky-local-variable t)

(defvar-local manner-line-buffer-identification-segment
    (propertized-buffer-identification "%b")
  "Stripped mode-line buffer identification.")

(put 'manner-line-buffer-identification-segment 'risky-local-variable t)

(defvar-local manner-line-buffer-status-segment
    `(:eval
      (if (get-buffer-process (current-buffer))
          ":%s on"
        '(:propertize
          (""
           mode-line-mule-info
           mode-line-modified
           mode-line-client
           mode-line-remote)
          display (min-width (6.0)))))
  "Buffer status `mode-line' segment.")

(put 'manner-line-buffer-status-segment 'risky-local-variable t)

(defun manner-line--compile-format-cell (side segment)
  "Compile SEGMENT for SIDE of `mode-line'."
  (let ((left-spacer (and (eq side 'right) manner-line-spacer))
        (right-spacer (and (eq side 'left) manner-line-spacer)))
    (remq nil (pcase segment
                (`(,symbol ,then ,else) (list "" left-spacer (list symbol then else) right-spacer))
                (`(,symbol . ,value) (list "" (list symbol (remq nil (list left-spacer value right-spacer)))))
                (symbol (list "" left-spacer symbol right-spacer))))))

(define-error
 'manner-line-invalid-flat-plist
 "[manner-line] Invalid flat plist for normalization"
 '(manner-line-error wrong-type-argument))

(defun manner-line--normalize-plist (plist)
  "Group flat values into a classical PLIST.

e.g. (:foo 1 2 :bar 3) => (:foo (1 2) :bar (3))"
  (let (current-keyword normalized-plist)
    (dolist (value (append plist '(:sentinel)))
      (if (keywordp value)
          (progn
            (let* ((current-value (plist-get normalized-plist current-keyword))
                   (current-car (car current-value)))
              (when (and (length= current-value 1) (listp current-car))
                (setq
                 normalized-plist
                 (plist-put normalized-plist current-keyword current-car))))
            (setq current-keyword value))
        (when (not current-keyword)
          (signal 'manner-line-invalid-flat-plist (list plist)))
        (let ((current-value (plist-get normalized-plist current-keyword)))
          (setq
           normalized-plist
           (plist-put normalized-plist current-keyword (append current-value (list value)))))))
    normalized-plist))

;;;###autoload
(defun manner-line-format ()
  "Generalte `mode-line-format' from `manner-line-format'."
  (let ((segment-plist (manner-line--normalize-plist manner-line-format)))
    `(,manner-line-header
      ,@(mapcar (apply-partially #'manner-line--compile-format-cell 'left) (plist-get segment-plist :left))
      mode-line-format-right-align
      ,@(mapcar (apply-partially #'manner-line--compile-format-cell 'right) (plist-get segment-plist :right))
      ,manner-line-footer)))

;;;###autoload
(define-minor-mode manner-line-mode
  "Toggle manner-line management of mode-line."
  :group 'manner-line
  :global t
  :lighter nil
  (if manner-line-mode
      (progn
        (when manner-line-hook-alist
          (manner-line-enable-feature-hooks 'custom manner-line-hook-alist))
        (when manner-line-masked-symbols
          (manner-line-enable-feature-mask 'custom manner-line-masked-symbols))
        (dolist (feature manner-line-features)
          (manner-line-enable-feature feature))
        (setq-default mode-line-format (manner-line-format)))
    (dolist (feature manner-line-features)
      (manner-line-disable-feature feature))
    (when manner-line-masked-symbols
      (manner-line-disable-feature-mask 'custom))
    (when manner-line-hook-alist
      (manner-line-disable-feature-hooks 'custom))))

;;;###autoload
(defun manner-line-enable ()
  "Unconditionally enable manner-line-mode."
  (interactive)
  (manner-line-mode +1))

;;;###autoload
(defun manner-line-disable ()
  "Unconditionally disable manner-line-mode."
  (interactive)
  (manner-line-mode -1))

(provide 'manner-line)

;;; manner-line.el ends here
