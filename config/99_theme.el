;;; 99_theme.el --- load emacs theme -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package doom-modeline-core
  :ensure nil

  :custom
  (doom-modeline-project-detection 'projectile)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-lsp-icon t)
  (doom-modeline-time-icon nil)
  (doom-modeline-time-live-icon nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-buffer-name t)
  (doom-modeline-column-zero-based t)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-default-coding-system 'utf-8)
  (doom-modeline-default-eol-type 0)
  (doom-modeline-total-line-number t)
  (doom-modeline-modal nil)
  (doom-modeline-modal-icon nil)
  (doom-modeline-modal-modern-icon t)
  (doom-modeline-irc nil))

(use-package doom-themes
  :custom
  (doom-themes-padded-modeline t)
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)

  :config
  (doom-themes-visual-bell-config))

(use-package doom-gruvbox-theme
  :ensure nil

  :custom
  (doom-gruvbox-brighter-comments nil)
  (doom-gruvbox-dark-variant "hard")

  :config
  (load-theme 'doom-gruvbox t))

(use-package doom-themes-ext-treemacs
  :ensure nil

  :custom
  (doom-themes-treemacs-theme "doom-colors"))

;;; 99_theme.el ends here
