;;; config/99_theme.el --- load emacs theme

;; Copyright (C) 2012-2018 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(add-to-list 'custom-theme-load-path package-user-dir)

(setq
 custom-safe-themes
 '("190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df"))

(load-theme 'zenburn)

;;; config/99_theme.el ends here
