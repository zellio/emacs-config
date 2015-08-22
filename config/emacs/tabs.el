
;;; tabs.el --- TAB Configuration

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq-default indent-tabs-mode 1)       ;; Turn off '\t' character
(setq-default standard-indent 4)          ;; Set indent to "  "
(setq-default tab-width 4)                ;; Set indent to "  "

(defun user:smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(global-set-key (kbd "TAB") 'user:smart-indent)

;; end of tabs.el
