;;; interface.el --- Vendored interaction packages  -*- lexical-binding: t -*-

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

(use-package all-the-icons)

(use-package all-the-icons-nerd-fonts
  :after all-the-icons nerd-icons
  :ensure (:host github :repo "mohkale/all-the-icons-nerd-fonts"))

(use-package cape
  :functions cape-dabbrev cape-file
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)

  :general
  ("C-c c" cape-prefix-map))

(use-package consult
  :after projectile
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :preface
  (defun user/update-minibuffer-contents (func)
    ""
    (lambda ()
      (interactive)
      (let* ((contents (minibuffer-contents-no-properties))
             (new-contents (funcall func contents)))
        (delete-minibuffer-contents)
        (insert new-contents))))

  (defun user/consult-goto-line-around (func &rest args)
    ""
    (let* ((minibuffer-local-map (copy-sequence minibuffer-local-map)))
      (general-define-key
       :keymaps 'minibuffer-local-map
       "C-p" (user/update-minibuffer-contents
              (lambda (x) (number-to-string (- (string-to-number x) 1))))
       "C-n" (user/update-minibuffer-contents
              (lambda (x) (number-to-string (+ (string-to-number x) 1)))))
      (apply func args)))

  :init
  (defvar-keymap user/consult-global-map
    "c" 'consult-locate
    "d" 'consult-find
    "e" 'consult-isearch-history
    ;; "f" 'consult-flycheck
    "g" 'consult-grep
    "G" 'consult-git-grep
    "i" 'consult-imenu
    "I" 'consult-imenu-multi
    "k" 'consult-kmacro
    "K" 'consult-keep-lines
    "l" 'consult-line
    "L" 'consult-line-multi
    "m" 'consult-mark
    "o" 'consult-outline
    "r" 'consult-ripgrep
    "s" 'consult-line-multi
    "u" 'consult-focus-lines
    "M-x" 'consult-mode-command)

  :general
  ([remap Info-search] 'consult-info
   [remap bookmark-jump] 'consult-bookmark
   [remap goto-line] 'consult-goto-line
   [remap pop-global-mark] 'consult-global-mark
   [remap project-switch-to-buffer] 'consult-project-buffer
   [remap repeat-complex-command] 'consult-complex-command
   [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame
   [remap switch-to-buffer-other-tab] 'consult-buffer-other-tab
   [remap switch-to-buffer-other-window] 'consult-buffer-other-window
   [remap switch-to-buffer] 'consult-buffer
   [remap yank] 'consult-yank-from-kill-ring
   [remap yank-pop] 'consult-yank-pop

   "C-S-s" 'consult-line
   "C-M-#" 'consult-register
   "C-x m" 'consult-mark
   "M-#" 'consult-register-load
   "M-'" 'consult-register-store
   "M-c" user/consult-global-map)

  (ctl-x-4-map
   [remap switch-to-buffer-other-window] 'consult-buffer-other-window)

  (ctl-x-5-map
   [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)

  (isearch-mode-map
   [remap isearch-edit-string] 'consult-isearch-history
   "M-s l" 'consult-line
   "M-s L" 'consult-line-multi)

  (minibuffer-local-map
   [remap next-matching-history-element] 'consult-history
   [remap previous-matching-history-element] 'consult-history

   ;; Emulate some helm bindings
   "<right>" #'minibuffer-complete-word
   "<left>" #'backward-kill-word)

  (projectile-command-map
   [remap projectile-switch-to-buffer] 'consult-project-buffer)

  :config (advice-add 'consult-goto-line :around #'user/consult-goto-line-around)
  :custom (consult-project-function #'projectile-project-root))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package corfu
  :hook (emacs-startup . global-corfu-mode)
  :general
  ("M-/" 'completion-at-point)

  (corfu-map
   [tab] #'corfu-next
   "TAB" #'corfu-next
   [backtab] #'corfu-previous)

  :custom
  (corfu-auto nil)
  (corfu-count 12)
  (corfu-max-width 128)
  (corfu-cycle t))

(use-package corfu-terminal
  :after corfu
  :preface
  (declare-function corfu-terminal-mode "corfu")

  (defun user/safe-enable-corfu-terminal-mode ()
    "Enable corfu-terminal-mode under tty."
    (unless (display-graphic-p)
      (corfu-terminal-mode +1)))

  :hook
  (corfu-mode . user/safe-enable-corfu-terminal-mode))

(use-package expand-region
  :general ("C-\\" 'er/expand-region)
  :custom
  (expand-region-autocopy-register "e")
  (expand-region-smart-cursor t))

(use-package kind-icon
  :after corfu
  :defines corfu-margin-formatters
  :functions kind-icon-margin-formatter
  :custom (kind-icon-default-face 'corfu-default)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package marginalia
  :hook (emacs-startup . marginalia-mode)
  :general (minibuffer-local-map "M-A" 'marginalia-cycle)
  :custom (marginalia-align 'left))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package projectile
  :preface
  (defun user/projectile-mode-line-function ()
    ""
    (let ((project-name (projectile-project-name)))
      (format "%s/%s" projectile-mode-line-prefix project-name)))

  :hook (emacs-startup . projectile-mode)
  :general
  (projectile-mode-map
   "s-p" 'projectile-command-map
   "C-c p" 'projectile-command-map)

  :custom
  (projectile-indexing-method 'alien)
  (projectile-enable-caching nil)
  (projectile-mode-line-prefix " Proj")
  (projectile-mode-line-function #'user/projectile-mode-line-function)
  (projectile-per-project-compilation-buffer t)
  (projectile-project-search-path
   (directory-files (user/home-path "repos") t "^[^.]")))

(use-package rainbow-mode
  :hook prog-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package treemacs
  :init
  (defvar-keymap user/treemacs-workspace-map
    "w" 'treemacs-switch-workspace
    "a" 'treemacs-add-project-to-workspace
    "c" 'treemacs-create-workspace
    "e" 'treemacs-edit-workspaces
    "n" 'treemacs-next-workspace
    "r" 'treemacs-remove-project-from-workspace
    "C-k" 'treemacs-remove-workspace)

  (defvar-keymap user/treemacs-global-map
    "t" 'treemacs-select-window
    "p" 'treemacs-projectile
    "P" 'treemacs-add-and-display-current-project
    "f" 'treemacs-find-file
    "b" 'treemacs-bookmark
    "w" user/treemacs-workspace-map)

  :general
  ([remap delete-other-windows] 'treemacs-delete-other-windows
   "C-x C-n" 'treemacs-select-window
   "C-c t" user/treemacs-global-map))

(use-package treemacs-customization
  :ensure nil
  :after treemacs
  :custom
  (treemacs-litter-directories '("/node_modules" "/.venv" "/.cask" "/.git"))
  (treemacs-move-forward-on-expand t)
  (treemacs-show-hidden-files t)
  (treemacs-hide-dot-git-directory t)
  (treemacs-no-delete-other-windows t))

(use-package treemacs-themes
  :ensure nil
  :after treemacs
  :commands (treemacs-load-theme))

(use-package treemacs-nerd-icons
  :after (treemacs treemacs-themes nerd-icons)
  :functions treemacs-load-theme
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package vertico
  :hook
  (emacs-startup . vertico-mode)
  (emacs-startup . vertico-multiform-mode)

  :defines vertico-multiform-categories
  :custom
  (vertico-count 12)
  (vertico-cycle t))

(use-package zoom
  :hook (emacs-startup . zoom-mode)
  :custom
  (zoom-size '(120 . 32))
  (zoom-ignored-major-modes '(flycheck-errors-list-mode))
  (zoom-ignored-buffer-names nil)
  (zoom-ignored-buffer-name-regexps nil)
  (zoom-ignore-predicates nil)
  (zoom-minibuffer-preserve-layout t))

(provide 'config/vendor/interface)

;;; interface.el ends here
