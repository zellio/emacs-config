;;; config/emacs/tabs.el --- Tab Configuration

;; Copyright (C) 2012-2016 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq-default
 indent-tabs-mode t
 standard-indent 4
 tab-width 4)

(defun user/smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(global-set-key (kbd "TAB") 'user/smart-indent)

;;; config/emacs/tabs.el ends here
