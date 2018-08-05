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
 ("3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077"
  "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df"))

(load-theme 'zenburn)

;;; config/99_theme.el ends here
