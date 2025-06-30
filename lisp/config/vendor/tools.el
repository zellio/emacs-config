;;; tools.el --- vendored tool package configuration -*- lexical-binding: t; coding: utf-8-unix; -*-

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

(eval-when-compile
  (require 'config/emacs))

(use-package asdf-vm
  :ensure nil (:host github :repo ("zellio/emacs-asdf-vm" . "asdf-vm"))
  :hook (emacs-startup . asdf-vm-mode-enable)
  :custom
  (asdf-vm-path-injection-behaviour nil)
  (asdf-vm-installer-src-dir "/Users/z.elliott/.local/share/asdf/downloads/asdf")
  (asdf-vm-installer-bin-dir "/Users/z.elliott/.local/bin"))

(use-package bazel
  :custom
  (bazel-command-options
   (list
    "--tool_tag=emacs"
    (format "--bazelrc=%s" (user/home-path ".config/bazel/bazelrc")))))

(use-package cargo
  :after (rust-ts-mode)
  :hook (rust-ts-mode . cargo-minor-mode))

(use-package flycheck
  :defines flycheck--automatically-enabled-checkers
  :hook (emacs-startup . global-flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(python-pylint))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-mode-line nil)
  (flycheck-temp-prefix ".flycheck")

  :config
  (setq
   flycheck--automatically-enabled-checkers '(python-flake8 python-mypy)))

(use-package flycheck-eglot
  :after flycheck eglot
  :preface (user/defun-enable-mode global-flycheck-eglot-mode)
  :functions global-flycheck-eglot-mode
  :hook (emacs-startup . enable-global-flycheck-eglot-mode))

(use-package flycheck-rust
  :after (flycheck rust-ts-mode)
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package transient)

(use-package magit
  :after transient
  :init
  (defvar-keymap user/magit-global-map
    "b" 'magit-blame
    "g" 'magit-dispatch
    "f" 'magit-file-dispatch
    "p" 'magit-process-mode
    "s" 'magit-status)
  :general ("C-c m" user/magit-global-map)
  :custom (magit-define-global-key-bindings nil))

(use-package magit-apply
  :ensure nil
  :after magit
  :custom (magit-delete-by-moving-to-trash nil))

(use-package magit-branch
  :ensure nil
  :after magit
  :custom
  (magit-branch-prefer-remote-upstream t)
  (magit-published-branches
   '("origin/master" "origin/main" "upstream/master" "upstream/main")))

(use-package magit-pull
  :ensure nil
  :after magit
  :custom
  (magit-pull-or-fetch t))

(use-package pipenv
  :after python-ts-mode
  :functions pipenv-project? pipenv-activate pipenv-deactivate
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

  :hook (python-ts-mode . user/pipenv-maybe-activate)
  :custom
  (pipenv-executable (executable-find "pipenv"))
  (pipenv-process-name "pipenv")
  (pipenv-process-buffer-name "*pipenv*")
  (pipenv-shell-buffer-name "pipenv shell")
  (pipenv-projectile-after-switch-function nil))


(use-package poetry
  :after (python-ts-mode projectile)
  :functions poetry-tracking-mode
  :hook (python-ts-mode . enable-poetry-tracking-mode)
  :preface (user/defun-enable-mode poetry-tracking-mode)
  :commands (poetry-find-project-root)
  :custom
  (poetry-virtualenv-path (or (getenv "POETRY_VIRTUALENVS_PATH")
                              (when-let* ((xdg-cache-dir (getenv "XDG_CACHE_DIR")))
                                (expand-file-name "virtualenvs" xdg-cache-dir))
                              (expand-file-name ".venv" (getenv "HOME"))))
  (poetry-tracking-strategy 'projectile))

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

(use-package vterm)

(use-package yasnippet
  :diminish yas-minor-mode
  :preface (user/defun-enable-mode yas-global-mode)
  :functions yas-global-mode
  :hook (emacs-startup . enable-yas-global-mode))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package yasnippet-capf
  :after (yasnippet cape)
  :functions yasnippet-capf
  :config (add-hook 'completion-at-point-functions #'yasnippet-capf))

(provide 'config/vendor/tools)

;;; tools.el ends here
