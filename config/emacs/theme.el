;;; config/emacs/theme.el --- load emacs theme

;; Copyright (C) 2012-2016 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(add-to-list 'custom-theme-load-path package-user-dir)

(setq
 custom-safe-themes
 '("427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55"))

(load-theme 'dracula)

;;; config/emacs/theme.el ends here
