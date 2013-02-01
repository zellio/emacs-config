
;;; flyspell.el --- flyspell-mode Configuration

;; Copyright (C) 2012 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(dolist (mode-hook '(markdown-mode-hook
                     org-mode-hook
                     text-mode-hook))
  (add-hook mode-hook (lambda () (flyspell-mode 1))))

;; end of flyspell.el
