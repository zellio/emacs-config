;;; config/99_theme.el --- load emacs theme

;; Copyright (C) 2012-2020 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:


(use-package zenburn-theme
  :init
  (setq
   custom-safe-themes
   '("d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298"))

  :config
  (add-to-list 'custom-theme-load-path package-user-dir)
  (load-theme 'gruvbox))

;;; config/99_theme.el ends here
