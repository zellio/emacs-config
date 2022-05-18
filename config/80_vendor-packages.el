;;; config/80_site-package.el --- installed package configurations

;; Copyright (C) 2012-2022 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:


;;; Rainbow Delimiters Mode
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;; Expand Region
(use-package expand-region
  :bind ("C-\\" . er/expand-region))


;;; Doom Modeline
(use-package doom-modeline
  :init (doom-modeline-mode t)

  :config
  (setq
   doom-modeline-unicode-fallback t
   doom-modeline-buffer-file-name-style 'truncate-upto-project))

(use-package doom-themes
  :after (doom-modeline)

  :config
  (setq
   doom-themes-enable-bold t
   doom-themes-enable-italic t
   doom-themes-treemacs-theme "doom-colors")

  (load-theme 'doom-gruvbox t)

  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))


;;; ANSI Colours
(use-package ansi-color
  :hook
  ((compilation-filter . (lambda ()
                           (let ((buffer-read-only nil))
                             (ansi-color-apply-on-region compilation-filter-start (point)))))
   (shell-mode . ansi-color-for-comint-mode-on))

  :config
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))


;;; All the Icons
(use-package all-the-icons
  :if (display-graphic-p))


;;; Treemacs
(use-package treemacs
  :bind (:map global-map
         ([remap delete-other-windows] . treemacs-delete-other-windows)

         ("C-x C-n" . treemacs-select-window)
         ("C-c t t" . treemacs-select-window)
         ("C-c t p" . treemacs-projectile)
         ("C-c t P" . treemacs-add-and-display-current-project)
         ("C-c t f" . treemacs-find-file)
         ("C-c t b" . treemacs-bookmark)

         ("C-c t w w" . treemacs-switch-workspace)
         ("C-c t w a" . treemacs-add-project-to-workspace)
         ("C-c t w c" . treemacs-create-workspace)
         ("C-c t w e" . treemacs-edit-workspaces)
         ("C-c t w n" . treemacs-next-workspace)
         ("C-c t w r" . treemacs-remove-project-from-workspace)
         ("C-c t w C-k" . treemacs-remove-workspace)))

(use-package treemacs-customization
  :ensure nil

  :config
  (setq
   treemacs-indent-guide-style 'block
   treemacs-sorting 'alphabetic-case-insensitive-asc))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))


;;; Projectile
(use-package projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  (setq
   projectile-indexing-method 'alien
   projectile-mode-line-prefix "℘"
   projectile-per-project-compilation-buffer t
   projectile-project-search-path '("~/repos/zellio" "~/repos/TrialSpark")))

(use-package helm-projectile
  :after (projectile helm))


;;; Magit
(use-package magit
  :bind (("C-c m b" . magit-blame)
         ("C-c m d" . magit-dispatch)
         ("C-c m f" . magit-file-dispatch)
         ("C-c m p" . magit-process-mode)
         ("C-c m s" . magit-status)))


;;; Yasnippet
(use-package yasnippet)


;;; Flycheck
(use-package flycheck
  :config
  (setq flycheck-temp-prefix ".flycheck")
  (global-flycheck-mode))

;; (use-package flycheck-pycheckers
;;   :after (flycheck)
;;   :hook ((flycheck-mode . flycheck-pycheckers-setup)))


;;; Markdown Mode
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :config
  (setq
   markdown-indent-on-enter nil)

  (set-face-attribute 'markdown-code-face nil :inherit 'org-block))


;;; YAML Mode
(use-package yaml-mode
  :mode (("\\.jinja2?\\'" . yaml-mode))

  :config (setq
           yaml-block-literal-search-lines 512))


;; Bison/Flex Mode
(use-package bison-mode)


;;; Dockerfile Mode
(use-package dockerfile-mode
  :init
  (put 'docker-image-name 'safe-local-variable #'stringp)
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)

  :config (setq
           dockerfile-build-args '("--pull" "--rm")))


;;; Terraform Mode
(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)
  :config (setq terraform-indent-level 2))


;;; Ruby
(use-package enh-ruby-mode
  :mode
  (("\\.rb\\'" . enh-ruby-mode)
   ("\\.ru\\'" . enh-ruby-mode)
   ("\\.rake\\'" . enh-ruby-mode)
   ("\\.gemspec\\'" . enh-ruby-mode)
   ("\\.jbuilder\\'" . enh-ruby-mode)
   ("\\.podspec\\'" . enh-ruby-mode)
   ("\\.thor\\'" . enh-ruby-mode)
   ("Gemfile\\'" . enh-ruby-mode)
   ("Rakefile\\'" . enh-ruby-mode)
   ("Podfile\\'" . enh-ruby-mode)
   ("Thorfile\\'" . enh-ruby-mode)
   ("Guardfile\\'" . enh-ruby-mode)
   ("Vagrantfile\\'" . enh-ruby-mode)
   ("Capfile\\'" . enh-ruby-mode))

  :config
  (setq
   ruby-insert-encoding-magic-comment nil
   enh-ruby-indent-level 2
   enh-ruby-add-encoding-comment-on-save nil
   enh-ruby-deep-indent-paren nil
   enh-ruby-bounce-deep-indent t
   enh-ruby-hanging-indent-level 2))

(use-package inf-ruby
  :config (setq inf-ruby-console-environment "pry"))

(use-package rubocop)

(use-package rvm
  :hook ((ruby-mode enh-ruby-mode) . rvm-activate-corresponding-ruby)
  :config (setq rvm--gemset-default "emacs"))

(use-package robe
  :config (with-eval-after-load "company"
            (push 'company-robe company-backends)))

(use-package bundler)


;;; Rust
(use-package rust-mode
  :config
  (setq
   rust-format-on-save t))

(use-package flycheck-rust
  :after (flycheck rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup)
  :config
  (setq
   flycheck-rust-clippy-executable "~/.cargo/bin/cargo"))

(use-package racer
  :after (rust-mode)
  :hook (rust-mode . racer-mode))

(use-package cargo
  :after (rust-mode)
  :hook (rust-mode . cargo-minor-mode))


;; Groovy
(use-package groovy-mode
  :mode (("Jenkinsfile\\'" . groovy-mode))
  :config (setq
           lsp-groovy-server-file "/Users/zellio/repos/GroovyLanguageServer/groovy-language-server/build/libs/groovy-language-server-all.jar"))


;;; Python
;; (use-package python-mode
;;   :hook
;;   ((python-mode . (lambda ()
;;                    (setq-local flycheck-checker 'python-pylint)
;;                    (flycheck-select-checker 'python-pylint)))))

(use-package pipenv
  :after (python-mode)

  :hook ((python-mode . (lambda ()
                          (unless (and (boundp 'user/pipenv-dir-cache)
                                       (string= (pipenv-project?) user/pipenv-dir-cache))
                            (setq user/pipenv-dir-cache (pipenv-project?))
                            (pipenv-deactivate)
                            (pipenv-activate))))
         (python-mode . (lambda ()
                          (when (pipenv-project-p)
                            (setq-local lsp-pyls-server-command '("pipenv" "run" "pylsp")))))
         (python-mode . pipenv-mode))


  :config
  (setq
   pipenv-executable "/Users/zellio/.pyenv/shims/pipenv"
   pipenv-projectile-after-switch-function nil)

  :commands (pipenv-mode
             pipenv-activate
             pipenv-run))

(use-package poetry
  :after (python-mode)
  :commands (poetry-find-project-root)
  :hook ((python-mode . (lambda () (poetry-tracking-mode +1)))
         (python-mode . (lambda ()
                          (message "Hello world")
                          (when (poetry-find-project-root)
                            (setq-local lsp-pyls-server-command
                                        '("poetry" "run" "pylsp")))))))


;;; protobuff
(use-package protobuf-mode
  :ensure nil
  :after (lsp-mode)

  :config
  (add-to-list 'lsp-language-id-configuration '(protobuf-mode . "protobuf"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("/Users/zellio/repos/micnncim/protocol-buffers-language-server/bazel-bin/cmd/protocol-buffers-language-server/darwin_amd64_stripped/protocol-buffers-language-server"))
                    :major-modes '(protobuf-mode)
                    :activation-fn (lsp-activate-on "protobuf")
                    :priority 1
                    :server-id 'protobuf-ls)))


;;; LSP
(use-package lsp-mode
  :init
  (setq
   lsp-keymap-prefix "C-c l")

  :hook (((c++-mode
           c-mode
           c-or-c++-mode
           enh-ruby-mode
           groovy-mode
           js-jsx-mode
           js-mode
           lua-mode
           python-mode
           rust-mode
           terraform-mode
           typescript-mode) . lsp-deferred)
         (lsp-mode . yas-minor-mode))

  :commands lsp

  :config
  (setq
   lsp-log-io t

   lsp-message-project-root-warning t
   lsp-auto-configure t
   lsp-enable-snippet t

   lsp-client-packages (delete 'lsp-steep lsp-client-packages)
   lsp-client-packages (delete 'pyls lsp-client-packages))

  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui
  :after (lsp-mode))

(use-package lsp-ui-sideline
  :ensure nil
  :after (lsp-ui)
  :config
  (setq
   lsp-ui-sideline-show-diagnostics t
   lsp-ui-sideline-show-hover t
   lsp-ui-sideline-show-code-actions nil
   ;; lsp-ui-sideline-update-mode 'line
   lsp-ui-sideline-delay 0.5))

(use-package lsp-ui-peek
  :ensure nil
  :after (lsp-ui)
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))

  :config
  (setq
   lsp-ui-peek-enable t
   lsp-ui-peek-show-directory t))

(use-package lsp-ui-doc
  :ensure nil
  :after (lsp-ui)
  :config
  (setq
   lsp-ui-doc-enable t
   lsp-ui-doc-position 'top
   lsp-ui-doc-alignment 'frame
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-doc-show-with-mouse t))

(use-package lsp-ui-imenu
  :ensure nil
  :after (lsp-ui)
  :config
  (setq
   lsp-ui-imenu-enable t
   lsp-ui-imenu-kind-position 'top))


;;; Helm LSP
(use-package helm-lsp
  :after (lsp-mode)
  :commands helm-lsp-workspace-symbol

  :bind
  (:map lsp-mode-map
        ([remap xref-find-apropos] . helm-lsp-workspace-symbol)))


;;; Rust LSP
(use-package lsp-rust
  :after (lsp-mode)
  :ensure nil
  :config
  (setq
   lsp-rust-analyzer-server-display-inlay-hints t
   lsp-rust-analyzer-inlay-hints-mode t
   lsp-rust-analyzer-cargo-watch-enable t
   lsp-rust-analyzer-cargo-watch-command "clippy"
   lsp-rust-analyzer-cargo-run-build-scripts t
   lsp-rust-analyzer-proc-macro-enable t
   lsp-rust-analyzer-display-chaining-hints t
   lsp-rust-analyzer-display-parameter-hints t))


;;; Terraform LSP
(use-package lsp-terraform
  :ensure nil
  :after (lsp-mode)

  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
                    :major-modes '(terraform-mode)
                    :priority 1
                    :server-id 'terraform-ls)))


;;; LPS pylsp
(use-package lsp-pylsp
  :ensure nil
  :after (lsp-mode)
  :config
  (put
   'lsp-pylsp-plugins-pylint-args
   'safe-local-variable
   (lambda (arg)
     (and (vectorp arg)
          (seq-reduce (lambda (a b) (and a (char-or-string-p b))) arg t))))

  (put
   'lsp-pylsp-plugins-flake8-enabled
   'safe-local-variable
   (lambda (arg)
     (booleanp arg)))

  (setq
   lsp-pylsp-plugins-jedi-completion-fuzzy t
   lsp-pylsp-plugins-mccabe-threshold 10
   lsp-pylsp-plugins-pylint-enabled t
   lsp-pylsp-plugins-yapf-enabled t))


;;; Company
(use-package company
  :bind (:map company-active-map
         ("C-j" . nil)
         ("SPC" . (lambda () (interactive) (company-complete-selection) (insert " ")))
         ("TAB" . company-complete-common-or-cycle)
         ("<tab>" . company-complete-common-or-cycle)
         ("S-TAB" . company-select-previous)
         ("<backtab>" . company-select-previous))

  :hook (after-init . global-company-mode)

  :config
  (setq
   company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-frontend
                       company-echo-metadata-frontend)
   company-tooltip-limit 10
   company-idle-delay 0.25
   company-minimum-prefix-length 2
   company-require-match 'never
   company-show-numbers t
   company-enable-lsp-snippet t
   company-tooltip-align-annotations t
   lsp-completion-provider :capf)

  (add-to-list 'company-backends #'company-capf))

(use-package company-terraform
  :after (company)
  :config (progn
            (require 'company-terraform)
            (company-terraform-init)))

(use-package company-racer)


;;; config/80_site-package.el ends here
