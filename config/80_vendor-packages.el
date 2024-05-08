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
  :preface
  (defun user/smart-complete ()
    ""
    (interactive)
    (let* ((current-point (point))
           (current-tick (buffer-chars-modified-tick)))
      (catch 'break
        (dolist (completion-func '(yas-expand company-complete-common-or-cycle))
          (ignore-errors (funcall completion-func))
          (unless (and (eq (point) current-point)
                       (eq (buffer-chars-modified-tick) current-tick))
            (throw 'break nil))))
      ))

  :hook (after-init . global-company-mode)

  :general
  ("M-/" 'company-complete)

  (company-mode-map
   [remap indent-for-tab-command] 'company-indent-or-complete-common)

  (company-active-map
   [tab] 'user/smart-complete
   "TAB" 'user/smart-complete
   [backtab] 'company-select-previous-or-abort)

  :custom
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-if-just-one-frontend
                       company-echo-metadata-frontend))
  (company-tooltip-limit 10)
  (company-tooltip-minimum 4)
  (company-tooltip-offset-display 'lines)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-backends
   '(company-capf company-files (company-gtags company-etags) company-dabbrev))
  (company-minimum-prefix-length 2)
  (company-abort-manual-when-too-short nil)
  (company-abort-on-unique-match t)
  (company-require-match 'never)
  (company-idle-delay 0.1)
  (company-tooltip-align-annotations '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
  (company-show-quick-access t)
  (company-selection-wrap-around t)
  (company-lighter-base "Cmpy")

  :config
  (setq
   company-lighter '(" "
                     company-lighter-base
                     (company-candidates
                      (:eval
                       (format
                        "/%s"
                        (replace-regexp-in-string
                         "company-\\|-company" "" (symbol-name company-backend))))
                      ))
   ))

(use-package consult
  :after (projectile)

  :hook (completion-list-mode . consult-preview-at-point-mode)

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
  (consult-project-function #'projectile-project-root))

(use-package consult-eglot
  :after (consult eglot))

(use-package consult-flycheck
  :after (consult flycheck))

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

(use-package expand-region
  :general
  ("C-\\" 'er/expand-region)

  :custom
  (expand-region-autocopy-register "e")
  (expand-region-smart-cursor t))

(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(python-pylint))
  (flycheck-temp-prefix ".flycheck")
  (flycheck-mode-line nil)

  :config
  (setq
   flycheck--automatically-enabled-checkers '(python-flake8 python-mypy))

  (global-flycheck-mode))

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

(use-package magit-apply
  :ensure nil
  :custom
  (magit-delete-by-moving-to-trash nil))

(use-package magit-branch
  :ensure nil
  :custom
  (magit-branch-prefer-remote-upstream t)
  (magit-published-branches
   '("origin/master" "origin/main" "upstream/master" "upstream/main")))

(use-package magit-pull
  :ensure nil
  :custom
  (magit-pull-or-fetch t))

(use-package marginalia
  :hook (after-init . marginalia-mode)

  :general
  (minibuffer-local-map
   "M-A" 'marginalia-cycle)

  :custom
  (marginalia-align 'left))

(use-package pipenv
  :after (python-ts-mode)
  :hook (python-ts-mode . (lambda ()
                            (unless (and (boundp 'user/pipenv-dir-cache)
                                         (string= (pipenv-project?) user/pipenv-dir-cache))
                              (setq user/pipenv-dir-cache (pipenv-project?))
                              (pipenv-deactivate)
                              (pipenv-activate))))
  :custom
  (pipenv-executable (executable-find "pipenv"))
  (pipenv-process-name "pipenv")
  (pipenv-process-buffer-name "*pipenv*")
  (pipenv-shell-buffer-name "pipenv shell")
  (pipenv-projectile-after-switch-function nil))

(use-package plantuml-mode
  :mode (("\\.puml\\'" . plantuml-mode))
  :custom
  (plantuml-executable-path (executable-find "plantuml"))
  (plantuml-default-exec-mode 'executable)
  (plantuml-indent-level 8))

(use-package poetry
  :after (python-ts-mode projectile)
  :commands (poetry-find-project-root)
  :hook (python-ts-mode . (lambda () (poetry-tracking-mode +1)))
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

(use-package flycheck-rust
  :after (flycheck rust-ts-mode)
  :hook (flycheck-mode . flycheck-rust-setup)
  :config
  (setq
   flycheck-rust-clippy-executable (executable-find "cargo")))

(use-package cargo
  :after (rust-ts-mode)
  :hook (rust-ts-mode . cargo-minor-mode))

(use-package rainbow-mode
  :hook prog-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package scad-mode)

(use-package terraform-mode
  :custom
  (terraform-indent-level 2)
  (terraform-format-on-save-mode t)

  :config
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
  :custom
  (treemacs-litter-directories '("/node_modules" "/.venv" "/.cask" "/.git"))
  (treemacs-move-forward-on-expand t)
  (treemacs-show-hidden-files t)
  (treemacs-hide-dot-git-directory t)
  (treemacs-no-delete-other-windows t))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-count 12)
  (vertico-cycle t))

(provide '80_vendor-packages)

;;; 80_vendor-packages.el ends here
