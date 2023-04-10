;;; 99_theme.el --- load emacs theme -*- lexical-binding: t -*-

;; Copyright (C) 2012-2023 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(use-package gruvbox-theme
  :config
  (add-to-list 'custom-theme-load-path package-user-dir)
  (load-theme 'gruvbox t))

;;; 99_theme.el ends here
