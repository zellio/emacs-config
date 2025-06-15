;;; theme.el --- User theme -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Copyright (C) 2012-2025 Zachary Elliott

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>>
;; Version: 0.8.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package catppuccin-theme
  :custom (catppuccin-flavor 'mocha)
  :config (load-theme 'catppuccin :no-confirm))

(use-package manner-line
  :ensure nil
  :hook (emacs-startup . manner-line-enable)
  :custom
  (manner-line-features '(eglot flycheck project version-control))
  (manner-line-masked-symbols '(mode-line-format))
  (manner-line-format
   '(:left
     manner-line-buffer-identification-segment
     manner-line-position-segment
     :right
     manner-line-major-mode-segment
     (flycheck-mode . manner-line-flycheck-segment)
     (eglot--managed-mode . manner-line-eglot-segment)
     (vc-mode . manner-line-version-control-segment))))

(use-package manner-line-version-control
  :ensure nil
  :after manner-line nerd-icons
  :custom
  (manner-line-version-control-state-symbol-alist
   `((added . ,(nerd-icons-mdicon "nf-md-plus"))
     (edited . ,(nerd-icons-mdicon "nf-md-plus"))
     (needs-update . ,(nerd-icons-mdicon "nf-md-arrow_down"))
     (needs-merge . ,(nerd-icons-mdicon "nf-md-arrow_left_right"))
     (removed . ,(nerd-icons-codicon "nf-cod-error"))
     (conflict . ,(nerd-icons-codicon "nf-cod-error"))
     (missing . ,(nerd-icons-codicon "nf-cod-error"))
     (unregistered . ,(nerd-icons-codicon "nf-cod-error"))
     (up-to-date . ,(nerd-icons-mdicon "nf-md-check"))
     (ignored . ,(nerd-icons-mdicon "nf-md-check")))))

(use-package manner-line-flycheck
  :ensure nil
  :after manner-line nerd-icons
  :custom
  (manner-line-flycheck-symbol-alist
   `((not-checked . ,(nerd-icons-octicon "nf-oct-no_entry"))
     (no-checker . ,(nerd-icons-faicon "nf-fa-circle"))
     (running . ,(nerd-icons-faicon "nf-fa-rotate"))
     (errored . ,(nerd-icons-codicon "nf-cod-error"))
     (interrupted . ,(nerd-icons-mdicon "nf-md-sync_alert"))
     (suspicious .  ,(nerd-icons-codicon "nf-cod-question"))
     (finished . ,(nerd-icons-mdicon "nf-md-alpha_f_circle_outline")))))

(provide 'config/theme)

;;; theme.el ends here
