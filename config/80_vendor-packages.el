;;; config/80_site-package.el --- installed package configurations

;; Copyright (C) 2012-2020 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:


;; ;;; No Littering
(use-package no-littering
  :init (setq
         no-littering-var-directory
         (expand-file-name "data/" user-emacs-directory))

  :config
  (require 'recentf)

  (push 'no-littering-var-directory recentf-exclude)
  (push 'no-littering-etc-directory recentf-exclude)

  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))


;;; Rainbow Delimiters Mode
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;; Doom Modeline
(use-package doom-modeline
  :init (doom-modeline-mode t)

  :config
  (setq
   doom-modeline-window-width-limit 'truncate-upto-project
   doom-modeline-unicode-fallback t))


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

(use-package all-the-icons-dired
  :after (all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))


;;; Projectile
(use-package projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))

  :config
  (setq
   projectile-indexing-method 'alien
   projectile-per-project-compilation-buffer t
   projectile-mode-line-prefix "℘"))

(use-package helm-projectile
  :after (projectile helm))


;;; Compilation
(use-package compile
  :ensure nil
  :config (setq compilation-scroll-output t))


;;; Sr. Speedbar
(use-package sr-speedbar
  :bind
  (("C-x C-n" . sr-speedbar-toggle)
   ("s-s" . sr-speedbar-toggle))

  :config
  (setq
   sr-speedbar-right-side nil))


;;; Magit
(use-package magit
  :bind (("C-c m b" . magit-blame)
         ("C-c m d" . magit-dispatch)
         ("C-c m f" . magit-file-dispatch)
         ("C-c m p" . magit-process-mode)
         ("C-c m s" . magit-status)))


;;; Yasnippet
(use-package yasnippet)


;;; Flyspell Mode
(use-package flyspell
  :hook ((text-mode . (lambda () (flyspell-mode 1)))
         (prog-mode . flyspell-prog-mode))

  :config
  (setq
   ispell-program-name "aspell"
   ispell-extra-args '("--sug-mode=fast")))



;;; Flycheck
(use-package flycheck
  :config
  (setq
   flycheck-temp-prefix ".flycheck")

  (global-flycheck-mode))

(use-package flycheck-pycheckers
  :after (flycheck)
  :hook ((flycheck-mode . flycheck-pycheckers-setup)))


;;; Markdown Mode
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :config (setq
           markdown-indent-on-enter nil))


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


;; ;;; Lua Mode
;; (use-package lua-mode)


;;; Terraform Mode
(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)

  :config (setq
           terraform-indent-level 2))



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
  :config (setq
           inf-ruby-console-environment "pry"))

(use-package rubocop)

(use-package rvm
  :hook ((ruby-mode enh-ruby-mode) . rvm-activate-corresponding-ruby)

  :config (setq
           rvm--gemset-default "emacs"))

(use-package robe
  :config (with-eval-after-load "company"
            (push 'company-robe company-backends)))

(use-package bundler)


;;; Rust
(use-package rust-mode
  :config (setq
           rust-format-on-save t))

(use-package flycheck-rust
  :after (flycheck rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup))

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
           lsp-groovy-server-file "/Users/zellio/repos/prominic/groovy-language-server/build/libs/groovy-language-server-all.jar"))


;;; Python
(use-package python-mode)

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

  :config (setq
           pipenv-executable "/Users/zellio/.pyenv/shims/pipenv"
           pipenv-projectile-after-switch-function nil)

  :commands (pipenv-mode
             pipenv-activate
             pipenv-run))

(use-package poetry
  :after (python-mode)
  :commands (poetry-find-project-root)
  :hook ((python-mode . poetry-tracking-mode)
         (python-mode . (lambda ()
                          (when (poetry-find-project-root)
                            (setq-local lsp-pyls-server-command
                                        '("poetry" "run" "pylsp")))))))


;;; LSP
(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("s-l" . lsp-command-map)
              ("C-c l" . lsp-command-map))

  :hook (((c-mode-common
           lua-mode
           python-mode
           enh-ruby-mode
           rust-mode
           groovy-mode
           terraform-mode) . lsp-deferred)
         (lsp-mode . yas-minor-mode))

  :config
  (setq
   lsp-message-project-root-warning t
   lsp-auto-configure t
   lsp-enable-snippet t
   lsp-keymap-prefix "C-c l"

   lsp-client-packages (delete 'lsp-steep lsp-client-packages)
   lsp-clinet-packages (delete 'pyls lsp-client-packages)))

(use-package lsp-terraform
  :ensure nil
  :after (lsp-mode)

  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
                    :major-modes '(terraform-mode)
                    :priority 1
                    :server-id 'terraform-ls)))


(use-package lsp-ui
  :after (lsp)
  :hook (lsp-mode . lsp-ui-mode)

  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references))

  :config
  (setq
   lsp-ui-doc-enable t
   lsp-ui-flycheck-enable t
   lsp-ui-imenu-enable nil
   ;; lsp-ui-flycheck-enable t
   ;; lsp-ui-sideline-enable t
   lsp-ui-peek-enable t))


;;; Helm LSP
(use-package helm-lsp
  :after (lsp)
  :commands helm-lsp-workspace-symbol

  :bind
  (:map lsp-mode-map
        ([remap xref-find-apropos] . helm-lsp-workspace-symbol)))


;;; Company
(use-package company
  :bind (:map company-active-map
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
   lsp-completion-provider :capf)

  (add-to-list 'company-backends #'company-capf))

(use-package company-terraform
  :after (company)
  :config (progn
            (require 'company-terraform)
            (company-terraform-init)))

(use-package company-racer)


;;; config/80_site-package.el ends here
