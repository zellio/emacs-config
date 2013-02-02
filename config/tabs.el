
;;; tabs.el --- TAB Configuration

;; Copyright (C) 2012,2013 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq-default indent-tabs-mode nil)       ;; Turn off '\t' character
(setq-default standard-indent 2)          ;; Set indent to "  "
(setq-default tab-width 2)                ;; Set indent to "  "

(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(global-set-key (kbd "TAB") 'smart-indent)

;; end of tabs.el
