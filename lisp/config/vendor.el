;;; vendor.el --- Remote package configuration -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Copyright (C) 2012-2024 Zachary Elliott

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott
;; Version: 0.7.0
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

(use-package straight
  :custom
  ;; Enable automatic package installation
  (straight-use-package-by-default t))

;; Config

(use-package all-the-icons-nerd-fonts
  :after nerd-icons

  :straight
  (all-the-icons-nerd-fonts :host github :repo "mohkale/all-the-icons-nerd-fonts"))

(use-package bazel
  :custom
  (bazel-command-options
   (list
    "--tool_tag=emacs"
    (format "--bazelrc=%s" (user/home-path ".config/bazel/bazelrc")))))

(use-package bison-mode)

(use-package consult
  :after projectile

  :hook
  (completion-list-mode . consult-preview-at-point-mode)

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
    "f" 'consult-flymake
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

  :custom
  (consult-project-function #'projectile-project-root)

  :config
  (advice-add 'consult-goto-line :around #'user/consult-goto-line-around))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package corfu
  :hook
  (after-init . global-corfu-mode)

  :general
  ("M-/" 'completion-at-point)

  (corfu-map
   [tab] #'corfu-next
   "TAB" #'corfu-next
   [backtab] #'corfu-previous)

  :custom
  (corfu-auto t)
  (corfu-count 12)
  (corfu-max-width 128)
  (corfu-cycle t))

(use-package corfu-terminal
  :after corfu

  :preface
  (defun user/safe-enable-corfu-terminal-mode ()
    "Enable corfu-terminal-mode under tty."
    (unless (display-graphic-p)
      (corfu-terminal-mode +1)))

  :hook
  (corfu-mode . user/safe-enable-corfu-terminal-mode))

(use-package cape
  :functions
  cape-dabbrev
  cape-file

  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package cargo
  :after (rust-ts-mode)

  :hook
  (rust-ts-mode . cargo-minor-mode))

(use-package dockerfile-mode
  :init
  (put 'docker-image-name 'safe-local-variable #'stringp)
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)

  :custom
  (dockerfile-build-pull t)
  (dockerfile-build-args '("--rm"))
  (dockerfile-build-progress "plain")
  (dockerfile-use-buildkit t)
  (dockerfile-enable-auto-indent nil))

(use-package epkg
  :functions
  epkg-list-packages

  :config
  (defalias 'list-packages #'epkg-list-packages))

(use-package epkg-marginalia
  :after (epkg marginalia)

  :functions
  epkg-marginalia-annotate-package

  :defines
  marginalia-annotator-registry
  marginalia-command-categories

  :config
  (with-eval-after-load 'marginalia
    (setcar (alist-get 'package marginalia-annotator-registry) #'epkg-marginalia-annotate-package)
    (add-to-list 'marginalia-command-categories '(straight-use-package . package))))

(use-package expand-region
  :general
  ("C-\\" 'er/expand-region)

  :custom
  (expand-region-autocopy-register "e")
  (expand-region-smart-cursor t))

(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(python-pylint))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-mode-line nil)
  (flycheck-temp-prefix ".flycheck")

  :hook
  (after-init . global-flycheck-mode)

  :defines
  flycheck--automatically-enabled-checkers

  :config
  (setq
   flycheck--automatically-enabled-checkers '(python-flake8 python-mypy)))

(use-package kind-icon
  :after corfu

  :defines
  corfu-margin-formatters

  :functions
  kind-icon-margin-formatter

  :custom
  (kind-icon-default-face 'corfu-default)

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package magit
  :init
  (defvar-keymap user/magit-global-map
    "b" 'magit-blame
    "g" 'magit-dispatch
    "f" 'magit-file-dispatch
    "p" 'magit-process-mode
    "s" 'magit-status)

  :general
  ("C-c m" user/magit-global-map)

  :custom
  (magit-define-global-key-bindings nil))

(use-package flycheck-rust
  :after (flycheck rust-ts-mode)

  :hook
  (flycheck-mode . flycheck-rust-setup))

(use-package magit-apply
  :straight nil

  :custom
  (magit-delete-by-moving-to-trash nil))

(use-package magit-branch
  :straight nil
  :custom
  (magit-branch-prefer-remote-upstream t)
  (magit-published-branches
   '("origin/master" "origin/main" "upstream/master" "upstream/main")))

(use-package magit-pull
  :straight nil
  :custom
  (magit-pull-or-fetch t))

(use-package marginalia
  :hook
  (after-init . marginalia-mode)

  :general
  (minibuffer-local-map
   "M-A" 'marginalia-cycle)

  :custom
  (marginalia-align 'left))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :after nerd-icons

  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package org
  :mode
  ((rx ".org" line-end) . org-mode)

  :preface
  (defcustom user/org-directory
    (no-littering-expand-var-file-name "org")
    "Org file root directory."
    :type 'string
    :group 'user)

  (defcustom user/org-journal-file
    (expand-file-name "journal.org" user/org-directory)
    "Org journal file."
    :type 'string
    :group 'user)

  (defcustom user/org-incident-file
    (expand-file-name "incident.org" user/org-directory)
    ""
    :type 'string
    :group 'user)

  (defcustom user/org-default-notes-file
    (expand-file-name "notes.org" user/org-directory)
    "Org notest files."
    :type 'string
    :group 'user)

  (defun user/org-capture-template-todo ()
    (let ((lines '("* TODO %^{Description}%? %^g" ":LOGBOOK:"
                   "- Created                              %U"
                   ":END:")))
      (mapconcat #'identity lines "\n  ")))

  (defun user/org-capture-template-work-task ()
    (let ((lines '("* TODO %^{Type|CORE|SRE|DEVX|POMO}-%^{Ticket number} - %^{Description}%?"
                   ":PROPERTIES:"
                   ":LINK:     https://formationbio.atlassian.net/browse/%\\1-%\\2"
                   ":END:"
                   ":LOGBOOK:"
                   "- Created                              %U"
                   ":END:")))
      (mapconcat #'identity lines "\n  ")))

  (defun user/org-capture-template-oncall-task ()
    (let ((lines '("* TODO %^{Incident Id} - %^{Description}%?"
                   ":PROPERTIES:"
                   ":LINK:     https://10gen.pagerduty.com/incidents/%\\1"
                   ":END:"
                   ":LOGBOOK:"
                   "- Created                              %U"
                   ":END:")))
      (mapconcat #'identity lines "$\n  ")))

  (defun user/org-capture-template-journal ()
    (let ((lines '("* %^{Description}"
                   ":LOGBOOK:"
                   "- Captured %U"
                   ":END:"
                   "%?")))
      (mapconcat #'identity lines "\n  ")))

  :general
  ("C-c o a" 'org-agenda)
  ("C-c o r" 'org-capture)

  :hook
  (org-mode . (lambda ()
                (local-set-key [(control return)] 'org-insert-heading-after-current)))

  :custom
  (org-directory user/org-directory)
  (org-default-notes-file user/org-default-notes-file)
  (org-agenda-files (list user/org-directory))
  (org-M-RET-may-split-line nil)
  (org-log-done nil)
  (org-log-into-drawer t)
  (org-use-speed-commands t)
  (org-id-track-globally t)
  (org-highest-priority ?A)
  (org-default-priority ?C)
  (org-lowest-priority ?Z)

  (org-todo-keywords
   '((sequence "TODO(t)" "INPR(i!)" "|" "DONE(d@)")
     (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "OVER(c@/!)" "PHONE" "MEETING")
     (sequence "TODO" "INPR" "REVW" "TEST" "|" "DONE" "ARCH" "WONT")))

  (org-todo-keyword-faces
   '(("INPR" . (:foreground "steelblue" :weight bold))
     ("WAIT" . (:foreground "goldenrod" :weight bold))
     ("HOLD" . (:foreground "dark orange" :weight bold))
     ("MEETING" . (:foreground "forest green" :weight bold))
     ("PHONE" . (:foreground "forest green" :weight bold))))

  (org-todo-state-tags-triggers
   '(("OVER" ("OVER" . t))
     ("WAIT" ("WAIT" . t))
     ("HOLD" ("WAIT") ("HOLD" . t))
     (done ("WAIT") ("HOLD"))
     ("TODO" ("WAIT") ("OVER") ("HOLD"))
     ("INPR" ("WAIT") ("OVER") ("HOLD"))
     ("DONE" ("WAIT") ("OVER") ("HOLD"))))

  (org-capture-templates
   '(("t" "Todo"
      entry (file org-default-notes-file)
      (function user/org-capture-template-todo)
      :empty-lines-after 1
      :prepend t)

     ("w" "Work Todo"
      entry (file org-default-notes-file)
      (function user/org-capture-template-work-task)
      :empty-lines-after 1
      :prepend t)

     ("i" "Pagerduty Incident"
      entry (file user/org-incident-file)
      (function user/org-capture-template-oncall-task)
      :empty-lines-after 1
      :prepent t)

     ("j" "Journal"
      entry (file+olp+datetree user/org-journal-file)
      (function user/org-capture-template-journal)
      :empty-lines-before 1))
   ))

(use-package pipenv
  :after python-ts-mode

  :preface
  (defvar user/pipenv-dir-cache nil)

  (defun user/pipenv-maybe-activate ()
    ""
    (interactive)
    (when-let* ((pipenv-project (pipenv-project?)))
      (unless (and (boundp 'user/pipenv-dir-cache)
                   (string= pipenv-project user/pipenv-dir-cache))
        (setq
         user/pipenv-dir-cache pipenv-project)
        (pipenv-deactivate)
        (pipenv-activate))))

  :hook
  (python-ts-mode . user/pipenv-maybe-activate)

  :custom
  (pipenv-executable (executable-find "pipenv"))
  (pipenv-process-name "pipenv")
  (pipenv-process-buffer-name "*pipenv*")
  (pipenv-shell-buffer-name "pipenv shell")
  (pipenv-projectile-after-switch-function nil))

(use-package plantuml-mode
  :mode
  ((rx ".puml" line-end) . plantuml-mode)

  :custom
  (plantuml-executable-path (executable-find "plantuml"))
  (plantuml-default-exec-mode 'executable)
  (plantuml-indent-level 8))

(use-package poetry
  :after (python-ts-mode projectile)

  :commands (poetry-find-project-root)

  :hook
  (python-ts-mode . (lambda () (poetry-tracking-mode +1)))

  :custom
  (poetry-virtualenv-path (or (getenv "POETRY_VIRTUALENVS_PATH")
                              (when-let* ((xdg-cache-dir (getenv "XDG_CACHE_DIR")))
                                (expand-file-name "virtualenvs" xdg-cache-dir))
                              (expand-file-name ".venv" (getenv "HOME"))))
  (poetry-tracking-strategy 'projectile))

(use-package projectile
  :hook
  (after-init . (lambda () (projectile-mode +1)))

  :general
  (projectile-mode-map
   "s-p" 'projectile-command-map
   "C-c p" 'projectile-command-map)

  :custom
  (projectile-indexing-method 'alien)
  (projectile-enable-caching nil)
  (projectile-mode-line-prefix " Proj")

  (projectile-mode-line-function
   (lambda ()
     (let ((project-name (projectile-project-name)))
       (format "%s/%s" projectile-mode-line-prefix project-name))))

  (projectile-per-project-compilation-buffer t)
  (projectile-project-search-path
   (directory-files (user/home-path "repos") t "^[^.]")))

(use-package rainbow-mode
  :hook prog-mode)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package s
  :preface
  (defun user/snake-case-region (beg end)
    ""
    (interactive "*r")
    (replace-region-contents
     beg end
     (lambda () (s-snake-case (buffer-substring beg end)))))

  (defun user/upper-camel-case-region (beg end)
    ""
    (interactive "*r")
    (replace-region-contents
     beg end
     (lambda () (s-upper-camel-case (buffer-substring beg end)))))

  :general
  (mode-specific-map
   "l" 'user/snake-case-region
   "u" 'user/upper-camel-case-region))

(use-package scad-mode)

(use-package terraform-mode
  :hook
  (terraform-mode . eglot-ensure)

  :custom
  (terraform-indent-level 2)
  (terraform-format-on-save-mode t)

  :config
  (with-eval-after-load 'eglot
    (defvar eglot-server-programs)
    (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))))

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
  :straight nil
  :custom
  (treemacs-litter-directories '("/node_modules" "/.venv" "/.cask" "/.git"))
  (treemacs-move-forward-on-expand t)
  (treemacs-show-hidden-files t)
  (treemacs-hide-dot-git-directory t)
  (treemacs-no-delete-other-windows t))

(use-package treemacs-themes
  :straight nil
  :commands (treemacs-load-theme))

(use-package treemacs-nerd-icons
  :after (treemacs treemacs-themes nerd-icons)

  :functions
  treemacs-load-theme

  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package vertico
  :hook
  (after-init . vertico-mode)

  :custom
  (vertico-count 12)
  (vertico-cycle t))

(use-package undo-tree
  :diminish undo-tree-mode

  :functions
  global-undo-tree-mode

  :hook
  (after-init . (lambda () (global-undo-tree-mode 1)))

  :custom
  (undo-tree-strong-limit (* 1024 1024 128))
  (undo-tree-outer-limit (* 1024 1024 256))
  (undo-tree-enable-undo-in-region t)
  (undo-tree-history-directory-alist
   `(("." . ,(no-littering-expand-var-file-name "undo-tree-hist/"))))
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-lazy-drawing 128))

(use-package yasnippet
  :diminish yas-minor-mode

  :functions
  yas-global-mode

  :hook
  (after-init . (lambda () (yas-global-mode 1))))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package yasnippet-capf
  :after (yasnippet cape)

  :functions yasnippet-capf

  :config
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(use-package zoom
  :hook
  (after-init . zoom-mode)

  :custom
  (zoom-size '(120 . 32))
  (zoom-ignored-major-modes nil)
  (zoom-ignored-buffer-names nil)
  (zoom-ignored-buffer-name-regexps nil)
  (zoom-ignore-predicates nil)
  (zoom-minibuffer-preserve-layout t))

(provide 'config/vendor)

;;; vendor.el ends here
