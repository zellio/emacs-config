;;; config/pkg/auto-fill.el --- auto-fill configuration

;; Copyright (C) 2012-2016 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(dolist
	(mode-hook '(markdown-mode-hook
				 org-mode-hook
				 text-mode-hook
				 yaml-mode-hook))
  (add-hook mode-hook 'turn-on-auto-fill))

;;; config/pkg/auto-fill.el ends here
