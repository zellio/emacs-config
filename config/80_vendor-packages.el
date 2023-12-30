;;; 80_site-package.el --- installed package configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(use-package all-the-icons)

(use-package bison-mode)

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq
   company-lighter-base "𝕮"
   company-lighter '(" "
                     company-lighter-base
                     (company-candidates
                      (:eval
                       (format
                        ":%s"
                        (replace-regexp-in-string
                         "company-\\|-company" "" (symbol-name company-backend))))))
   company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-if-just-one-frontend
                       company-echo-metadata-frontend)
   company-tooltip-limit 10
   company-tooltip-minimum 5
   company-quick-access-keys '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
   company-quick-access-modifier 'meta
   company-tooltip-flip-when-above t
   company-idle-delay 0.25
   company-minimum-prefix-length 2
   company-require-match 'never
   company-show-numbers t))

(use-package dockerfile-mode
  :init
  (put 'docker-image-name 'safe-local-variable #'stringp)
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)

  :config
  (setq
   dockerfile-use-buildkit t
   dockerfile-build-pull t
   dockerfile-build-progress "plain"
   dockerfile-build-args '("--rm")))

(use-package eldoc-box
  :after (eldoc)
  :general ("C-c h" 'eldoc-box-help-at-point)
  :config
  (setq
   eldoc-box nil
   eldoc-box-only-multi-line t
   eldoc-box-clear-with-C-g t))

(use-package expand-region
  :general ("C-\\" 'er/expand-region))

(use-package flycheck
  :config
  (setq
   flycheck-temp-prefix ".flycheck"
   flycheck--automatically-enabled-checkers '(python-flake8 python-mypy)
   flycheck-disabled-checkers '(python-pylint))
  (global-flycheck-mode))

(use-package helm-projectile
  :after (projectile helm)
  :config (helm-projectile-on))

(use-package magit
  :general
  ("C-c m b" 'magit-blame
   "C-c m d" 'magit-dispatch
   "C-c m f" 'magit-file-dispatch
   "C-c m p" 'magit-process-mode
   "C-c m s" 'magit-status))

(use-package pipenv
  :after (python-ts-mode)
  :hook (python-ts-mode . (lambda ()
                             (unless (and (boundp 'user/pipenv-dir-cache)
                                          (string= (pipenv-project?) user/pipenv-dir-cache))
                               (setq user/pipenv-dir-cache (pipenv-project?))
                               (pipenv-deactivate)
                               (pipenv-activate))))
  :config
  (setq
   pipenv-executable "~/.local/bin/pipenv"
   pipenv-projectile-after-switch-function nil))

(use-package plantuml-mode
  :mode (("\\.puml\\'" . plantuml-mode))
  :config
  (setq
   plantuml-executable-path (executable-find "plantuml")
   plantuml-default-exec-mode 'executable
   plantuml-indent-level 8))

(use-package poetry
  :after (python-ts-mode projectile)
  :commands (poetry-find-project-root)
  :hook (python-ts-mode . (lambda () (poetry-tracking-mode +1)))
  :config
  (setq
   poetry-tracking-strategy 'projectile))

(use-package projectile
  :hook (after-init . (lambda () (projectile-mode +1)))
  :general
  (projectile-mode-map
   "s-p" 'projectile-command-map
   "C-c p"  'projectile-command-map)
  :config
  (setq
   projectile-indexing-method 'alien
   projectile-mode-line-prefix (char-to-string (c-int-to-char #x2119))
   projectile-mode-line-function
   (lambda ()
     (let ((project-name (projectile-project-name)))
       (format " %s:%s" projectile-mode-line-prefix project-name)))
   projectile-per-project-compilation-buffer t
   projectile-project-search-path '("~/repos/zellio" "~/repos/TrialSpark")))

(use-package rust-mode
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode rust-mode) .
                   ("rust-analyzer"
                    :initializationOptions (:check (:command "clippy")))))))

(use-package rust-rustfmt
  :ensure nil
  :after (rust-mode)
  :config
  (setq
   rust-format-on-save t
   rust-rustfmt-switches '("--edition" "2021")))

(use-package flycheck-rust
  :after (flycheck rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup)
  :config
  (setq
   flycheck-rust-clippy-executable (executable-find "cargo")))

(use-package cargo
  :after (rust-mode)
  :hook (rust-mode . cargo-minor-mode))

(use-package scad-mode)

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)
  :config
  (setq
   terraform-indent-level 2)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve"))))

  ;; TODO(gh://flycheck/flycheck/issues/2024): Remove once resolved upstream
  (with-eval-after-load 'flycheck
    (flycheck-define-checker terraform-tflint
      "A Terraform checker using tflint."
      :command ("tflint" "--format=json" "--force"
                (option-list "--var-file=" flycheck-tflint-variable-files concat)
                (eval (format "--chdir=%s" (file-name-directory (buffer-file-name)))))
      :error-parser flycheck-parse-tflint-linter
      :predicate flycheck-buffer-saved-p
      :modes terraform-mode)))

(use-package treemacs
  :general
  ([remap delete-other-windows] 'treemacs-delete-other-windows
   "C-x C-n" 'treemacs-select-window
   "C-c t t" 'treemacs-select-window
   "C-c t p" 'treemacs-projectile
   "C-c t P" 'treemacs-add-and-display-current-project
   "C-c t f" 'treemacs-find-file
   "C-c t b" 'treemacs-bookmark
   "C-c t w w" 'treemacs-switch-workspace
   "C-c t w a" 'treemacs-add-project-to-workspace
   "C-c t w c" 'treemacs-create-workspace
   "C-c t w e" 'treemacs-edit-workspaces
   "C-c t w n" 'treemacs-next-workspace
   "C-c t w r" 'treemacs-remove-project-from-workspace
   "C-c t w C-k" 'treemacs-remove-workspace))

(use-package treemacs-customization
  :ensure nil
  :config
  (setq
   treemacs-move-forward-on-expand t))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package yasnippet)

;;; 80_site-package.el ends here
