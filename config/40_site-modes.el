;;; 40_site-modes.el --- provided mode configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

;; Text Modes

(use-package flyspell
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode))

  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=fast"))
  (flyspell-mode-line-string nil))

(use-package toml-ts-mode
  :custom
  (toml-ts-mode-indent-offset user/indent-width))

;; Prog Modes

(use-package c-ts-mode
  :custom
  (c-ts-mode-indent-offset user/indent-width)
  (c-ts-mode-indent-style 'linux))

(use-package cmake-ts-mode
  :custom
  (cmake-ts-mode-indent-offset user/indent-width))

(use-package compile
  :custom
  (compilation-scroll-output t)
  (compilation-always-kill t)
  (compilation-max-output-line-length 1024))

(use-package cperl-mode
  :custom
  (cperl-indent-level user/indent-width))

(use-package eglot
  :ensure t
  :commands (eglot-managed-p)
  :hook
  ((c-ts-mode
    go-ts-mode
    js-ts-mode
    python-ts-mode
    rust-ts-mode
    typescript-ts-mode
    terraform-mode) . eglot-ensure)

  (before-save . (lambda ()
                   (when (eglot-managed-p)
                     (eglot-format-buffer))))

  :custom
  (eglot-connect-timeout 60)
  (eglot-sync-connect 3)
  (eglot-autoshutdown t)
  (eglot-ignored-server-capabilities '()))

(use-package gdb-mi
  :custom
  (gdb-debug-log-max 5120))

(use-package go-ts-mode
  :custom
  (go-ts-mode-indent-offset user/indent-width))

(use-package js
  :mode ("\\(\\.js[mx]?\\|\\.har\\)\\'" . js-ts-mode)

  :custom
  (js-indent-level 2)
  (js-indent-first-init 'dynamic))

(use-package json-ts-mode
  :custom
  (json-ts-mode-indent-offset 2))

(use-package python
  :preface
  (defun user/eglot-python-server (_)
    (cond
     ((and (fboundp 'poetry-find-project-root) (poetry-find-project-root))
      (list "poetry" "run" "pylsp"))
     ((and (fboundp 'pipenv-project-p) (pipenv-project-p))
      (list "pipenv" "run" "pylsp"))
     (t "pylsp")))

  :mode ("\\.py[iw]?\\'" . python-ts-mode)

  :custom
  (python-indent-offset 4)
  (python-shell-interpreter-args "-m IPython")

  :config
  (user/add-eglot-workspace-config :pylsp
   (:plugins
    (:autopep8 (:enabled :json-false)
     :flake8 (:enabled :json-false)
     :jedi (:auto_import_modules [] :env_vars nil :environment nil :extra_paths [])
     :jedi_completion (:enabled t :eager t :fuzzy t :include_class_objects t :include_function_objects t :include_params t)
     :jedi_definition (:enabled t :follow_builtin_definitions t :follow_builtin_imports t :follow_imports t)
     :jedi_hover (:enabled t)
     :jedi_references (:enabled t)
     :jedi_signature_help (:enabled t)
     :jedi_symbols (:enabled t :include_import_symbols t :all_scopes t)
     :mccable (:enabled :json-false)
     :preload (:enabled :json-false)
     :pycodestyle (:enabled :json-false)
     :pydocstyle (:enabled :json-false)
     :pyflakes (:enabled :json-false)
     :pylint (:enabled :json-false)
     :rope_autoimport (:enabled t :completions (:enabled t) :code_actions (:enabled t))
     :rope_completion (:enabled t :eager t)
     :yapf (:enabled :json-false))))

  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs '(python-ts-mode . user/eglot-python-server))))

(use-package rust-ts-mode
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode rust-mode) .
                   ("rust-analyzer"
                    :initializationOptions (:check (:command "clippy")))))))

(use-package sh-script
  :hook
  ((sh-mode . (lambda () (setq-local indent-tabs-mode t)))
   (bash-ts-mode . (lambda () (setq-local indent-tabs-mode t))))

  :config
  (setq
   auto-mode-alist (user/mask-auto-mode-alist 'sh-mode 'bash-ts-mode)))

(use-package typescript-ts-mode
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package yaml-ts-mode
  :hook
  (yaml-ts-mode . (lambda () (setq-local tab-width 2))))

;; Ensure Use Package

(use-package use-package-ensure
  :custom
  (use-package-always-ensure t))

;;; 40_site-modes.el ends here
