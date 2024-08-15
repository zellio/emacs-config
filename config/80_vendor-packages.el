;;; 80_site-package.el --- installed package configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(use-package bazel
  :custom
  (bazel-command-options '("--tool_tag=emacs" "--bazelrc=$HOME/.config/bazel/bazelrc")))

(use-package bison-mode)

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
  (consult-project-function #'projectile-project-root)

  :config
  (defun user/update-minibuffer-contents (func)
    ""
    (lambda ()
      (interactive)
      (let* ((contents (minibuffer-contents-no-properties))
             (new-contents (funcall func contents)))
        (delete-minibuffer-contents)
        (insert new-contents))
      ))

  (advice-add
   'consult-goto-line
   :around
   (lambda (func &rest args)
     (let* ((minibuffer-local-map (copy-sequence minibuffer-local-map)))
       (general-define-key
        :keymaps 'minibuffer-local-map
        "C-p" (user/update-minibuffer-contents
               (lambda (x) (number-to-string (- (string-to-number x) 1))))
        "C-n" (user/update-minibuffer-contents
               (lambda (x) (number-to-string (+ (string-to-number x) 1)))))
       (apply func args))
     )))

(use-package consult-eglot
  :after (consult eglot))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package corfu
  :hook (after-init . global-corfu-mode)

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
  :hook (corfu-mode . (lambda ()
                        (unless (display-graphic-p)
                          (corfu-terminal-mode +1)))))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package kind-icon
  :after corfu

  :custom
  (kind-icon-default-face 'corfu-default)

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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

(use-package nerd-icons)

(use-package nerd-icons-dired
  :after (nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package all-the-icons-nerd-fonts
  :after (nerd-icons)
  :config
  (package-install 'all-the-icons-nerd-fonts))

(use-package pipenv
  :after (python-ts-mode)

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
        (pipenv-activate))
      ))

  :hook (python-ts-mode . user/pipenv-maybe-activate)
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

(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . (lambda () (yas-global-mode 1))))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package yasnippet-capf
  :after (yasnippet cape)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-count 12)
  (vertico-cycle t))

(use-package zoom
  :hook (after-init . zoom-mode)
  :custom
  (zoom-size '(120 . 32))
  (zoom-ignored-major-modes nil)
  (zoom-ignored-buffer-names nil)
  (zoom-ignored-buffer-name-regexps nil)
  (zoom-ignore-predicates nil)
  (zoom-minibuffer-preserve-layout t))

(provide '80_vendor-packages)

;;; 80_vendor-packages.el ends here
