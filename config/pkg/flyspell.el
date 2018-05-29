;;; config/pkg/flyspell.el --- flyspell-mode configuration

;; Copyright (C) 2012-2017 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq
 ispell-list-command "--list"

 ;; for some reason this isn't getting set so set it here
 flyspell-delayed-commands '())

(dolist
    (mode-hook '(markdown-mode-hook
                 org-mode-hook
                 text-mode-hook
                 yaml-mode-hook))
  (add-hook mode-hook (lambda () (flyspell-mode t))))

;;; config/pkg/flyspell.el ends here
