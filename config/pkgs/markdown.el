
;;; markdown.el --- markdown Configuration

;; Copyright (C) 2012-2015 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-hook 'turn-on-auto-fill)

;;; markdown.el ends here
