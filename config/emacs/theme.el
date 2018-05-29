;;; config/emacs/theme.el --- load emacs theme

;; Copyright (C) 2012-2017 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(add-to-list 'custom-theme-load-path package-user-dir)

(setq
 custom-safe-themes
 '("a4df5d4a4c343b2712a8ed16bc1488807cd71b25e3108e648d4a26b02bc990b3"
   "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb"
   "d9129a8d924c4254607b5ded46350d68cc00b6e38c39fc137c3cfb7506702c12"
   "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366"
   "427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55"))

(load-theme 'dracula)

;;; config/emacs/theme.el ends here
