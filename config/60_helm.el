;;; config/60_helm.el --- global configurations

;; Copyright (C) 2012-2020 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

;;; init-helm.el --- My startup file for helm. -*- lexical-binding: t -*-
;;; Code:

;;; Set up helm first (will load helm-autoloads.el)

(use-package helm
  :config
  (require 'helm-config)

  (setq
   helm-input-idle-delay 0.01
   helm-reuse-last-window-split-state t
   helm-always-two-windows t
   helm-echo-input-in-header-line t
   helm-split-window-inside-p nil
   helm-actions-inherit-frame-settings t
   helm-use-frame-when-more-than-two-windows t
   helm-use-frame-when-dedicated-window t
   helm-frame-background-color "DarkSlateGray"
   helm-show-action-window-other-window 'left
   helm-allow-mouse t
   helm-move-to-line-cycle-in-source t
   helm-autoresize-max-height 80
   helm-autoresize-min-height 10
   helm-debug-root-directory (expand-file-name
                              "helm-debug" user/emacs-data-directory)
   helm-follow-mode-persistent t
   helm-candidate-number-limit 500
   helm-visible-mark-prefix "✓"
   helm-mode-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
   helm-turn-on-recentf t
   helm-commands-using-frame '(completion-at-point
                               helm-apropos
                               helm-eshell-prompts
                               helm-imenu
                               helm-imenu-in-all-buffers))
  (helm-autoresize-mode 1)
  (set-face-foreground 'helm-mark-prefix "Gold1")
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)
  (helm-define-key-with-subkeys global-map (kbd "C-c n") ?n 'helm-cycle-resume))


;;; Helm-mode is loading nearly everything.

(use-package helm-mode
  :ensure nil
  :init
  (add-hook 'helm-mode-hook
            (lambda ()
              (setq completion-styles
                    (cond ((assq 'helm-flex completion-styles-alist)
                           '(helm-flex)) ;; emacs-26.
                          ((assq 'flex completion-styles-alist)
                           '(flex)))))) ;; emacs-27+.
  :diminish (helm-mode " ⎈")
  :config
  (helm-mode 1)
  (setq
   helm-completing-read-handlers-alist
   '((write-file . helm-read-file-name-handler-1)
     (basic-save-buffer . helm-read-file-name-handler-1)
     (find-tag . helm-completing-read-default-find-tag)
     (xref-find-definitions . helm-completing-read-default-find-tag)
     (xref-find-references . helm-completing-read-default-find-tag)
     (ggtags-find-tag-dwim . helm-completing-read-default-find-tag)
     (tmm-menubar)
     (find-file)
     (execute-extended-command)
     (cancel-debug-on-entry)
     (org-capture . helm-org-completing-read-tags)
     (org-set-tags . helm-org-completing-read-tags)
     (dired-do-rename . helm-read-file-name-handler-1)
     (dired-do-copy . helm-read-file-name-handler-1)
     (dired-do-symlink . helm-read-file-name-handler-1)
     (dired-do-relsymlink . helm-read-file-name-handler-1)
     (dired-do-hardlink . helm-read-file-name-handler-1)
     (basic-save-buffer . helm-read-file-name-handler-1)
     (write-file . helm-read-file-name-handler-1)
     (write-region . helm-read-file-name-handler-1)))

  ;; Fix CAP with LSP in python.
  (add-to-list 'helm-completion-styles-alist '(python-mode . helm-fuzzy)))

(use-package helm-adaptive
  :ensure nil
  :config
  (setq helm-adaptive-history-file nil)
  (helm-adaptive-mode 1))

(use-package helm-utils
  :ensure nil
  :config
  ;; Popup buffer-name or filename in grep/moccur/imenu-all etc...
  (helm-popup-tip-mode 1)
  (setq helm-highlight-matches-around-point-max-lines '(30 . 30)
        helm-window-show-buffers-function #'helm-window-mosaic-fn)
  (add-hook 'find-file-hook 'helm-save-current-pos-to-mark-ring))

(use-package helm-sys
  :ensure nil
  :commands (helm-top)
  :config (helm-top-poll-mode 1))

(use-package helm-info
  :ensure nil
  :bind ("C-h r" . helm-info-emacs))

(use-package helm-ring
  :ensure nil
  :config
  (setq helm-kill-ring-threshold 1)
  :bind (:map helm-kill-ring-map
              ("C-d" . helm-kill-ring-run-persistent-delete)))

(use-package helm-buffers
  :ensure nil
  :config
  (setq helm-buffers-favorite-modes
        (append helm-buffers-favorite-modes '(picture-mode artist-mode))
        helm-buffers-fuzzy-matching t
        helm-buffer-skip-remote-checking t
        helm-buffer-max-length 22
        helm-buffers-end-truncated-string "…"
        helm-buffers-maybe-switch-to-tab t
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-buffer-not-found)
        helm-boring-buffer-regexp-list
        '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf"
          "\\`\\*Messages" "\\`\\*Magit" "\\`\\*git-gutter" "\\`\\*Help"))

  (define-key helm-buffer-map (kbd "C-d") 'helm-buffer-run-kill-persistent))

(use-package helm-files
  :ensure nil
  :config
  (setq helm-ff-auto-update-initial-value t
        helm-ff-allow-non-existing-file-at-point t
        helm-trash-remote-files t
        helm-dwim-target 'next-window
        helm-locate-recursive-dirs-command "fd --hidden --type d --glob '*%s*' %s"
        helm-ff-eshell-unwanted-aliases '("sudo" "cdu" "man" "gpg-pubkey-export-armor" "gpg-secretkey-export-armor"))
  (customize-set-variable 'helm-ff-lynx-style-map t)

  (define-key helm-read-file-map (kbd "RET") 'helm-ff-RET)
  (define-key helm-find-files-map (kbd "C-d") 'helm-ff-persistent-delete)
  (define-key helm-find-files-map (kbd "C-:") 'helm-ff-tramp-methods-complete)

  (defun helm/insert-date-in-minibuffer ()
    (interactive)
    (with-selected-window (or (active-minibuffer-window)
                              (minibuffer-window))
      (unless (or (helm-follow-mode-p)
                  helm--temp-follow-flag)
        (goto-char (point-max))
        (insert (format-time-string "%Y-%m-%d-%H:%M")))))

  (define-key helm-find-files-map (kbd "C-c y") 'helm/insert-date-in-minibuffer)
  (define-key helm-read-file-map (kbd "C-c y") 'helm/insert-date-in-minibuffer))

(use-package helm-descbinds
  :config
  ;; C-h b, C-x C-h etc...
  (helm-descbinds-mode 1))

(use-package helm-lib
  :ensure nil
  :config
  ;; (use-package isearch-light)
  (setq helm-scroll-amount 4)
  (setq helm-find-function-default-project
        '("~/labo/emacs/lisp/" "~/labo/github/"))
  (helm-help-define-key "C-x" 'exchange-point-and-mark)
  (helm-help-define-key "C-l" 'recenter-top-bottom)
  (helm-help-define-key "C-s" nil)
  (helm-help-define-key "C-r" nil)
  (helm-help-define-key "C-s" 'isl-search))

(use-package helm-grep
  :ensure nil
  :config
  (setq helm-pdfgrep-default-read-command
        "evince --page-label=%p '%f'"
        helm-grep-default-command
        "ack-grep -Hn --color --smart-case --no-group %e %p %f"
        helm-grep-default-recurse-command
        "ack-grep -H --color --smart-case --no-group %e %p %f"
        helm-grep-ag-command
        "rg --color=always --colors 'match:bg:yellow' --colors 'match:fg:black' --smart-case --no-heading --line-number %s %s %s"
        helm-grep-ag-pipe-cmd-switches
        '("--colors 'match:bg:yellow' --colors 'match:fg:black'")
        helm-grep-git-grep-command
        "git --no-pager grep -n%cH --color=always --exclude-standard --no-index --full-name -e %p -- %f")
  (add-hook 'helm-grep-mode-hook 'hl-line-mode)
  (define-key helm-grep-map (kbd "C-M-a") 'helm/occur-which-func))

(use-package helm-occur
  :ensure nil
  :config
  (setq helm-occur-keep-closest-position t)
  (add-hook 'helm-occur-mode-hook 'hl-line-mode)
  (define-key helm-occur-map (kbd "C-M-a") 'helm/occur-which-func))

(use-package helm-elisp
  :ensure nil
  :config
  (setq helm-show-completion-display-function #'helm-display-buffer-in-own-frame
        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t)
  (helm-multi-key-defun helm-multi-lisp-complete-at-point
      "Multi key function for completion in emacs lisp buffers.
First call indent, second complete symbol, third complete fname."
    '(helm-lisp-indent
      helm-lisp-completion-at-point
      helm-complete-file-name-at-point)
    0.3)
  (define-key emacs-lisp-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point)
  (define-key lisp-interaction-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point))

(use-package helm-locate
  :ensure nil
  :config
  (setq helm-locate-fuzzy-match t))

(use-package helm-org
  :config
  (setq helm-org-headings-fontify t))

(use-package helm-emms
  :after 'emms
  :config
  (setq helm-emms-use-track-description-function nil))

(use-package helm-find
  :ensure nil
  :config
  (setq helm-find-noerrors t))

(use-package helm-elisp-package
  :ensure nil
  :config
  (setq helm-el-package-initial-filter 'installed
        helm-el-package-autoremove-on-start t
        helm-el-package-upgrade-on-start t))

(use-package helm-imenu
  :ensure nil
  :config
  (add-to-list 'helm-imenu-type-faces '("^Use package$" . font-lock-keyword-face))
  (customize-set-variable 'helm-imenu-lynx-style-map t))

(use-package helm-misc
  :ensure nil
  :config
  ;; Minibuffer history (Rebind to M-s).
  (customize-set-variable 'helm-minibuffer-history-key [remap next-matching-history-element]))

(use-package helm-epa
  :ensure nil
  :config
  (helm-epa-mode 1))

(use-package helm-all-the-icons
  :ensure nil
  :commands helm-all-the-icons)

(use-package helm-ls-git
    :config
  (setq helm-ls-git-delete-branch-on-remote t))

(use-package helm-tramp
  :config
  (setq
   helm-tramp-control-master t
   helm-tramp-control-master-path "~/.ssh/socket"
   helm-tramp-control-master-prefix "")

  :bind
  (("C-c s" . helm-tramp)))

(use-package helm-lsp
  ;; Configured in lsp include
  )


;;; Bindings

;;; Ctl-x-5 map
;;
(define-key ctl-x-5-map (kbd "C-x c t") 'helm-top-in-frame)
(define-key ctl-x-5-map (kbd "C-x c i") 'helm-imenu-in-frame)
(define-key ctl-x-5-map (kbd "C-x C-f") 'helm-find-files-in-frame)
(define-key ctl-x-5-map (kbd "M-x") 'helm-M-x-in-frame)
(define-key ctl-x-5-map (kbd "C-s") 'helm-occur-in-frame)
(define-key ctl-x-5-map (kbd "C-x C-b") 'helm-mini-in-frame)

;;; Helm-command-map
;;
(define-key helm-command-map (kbd "z") 'helm-complex-command-history)
(define-key helm-command-map (kbd "#") 'helm-emms)
(define-key helm-command-map (kbd "I") 'helm-imenu-in-all-buffers)
(define-key helm-command-map (kbd "@") 'helm-list-elisp-packages-no-fetch)

;;; Global-map
;;
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map (kbd "C-c <SPC>") 'helm-all-mark-rings)
(define-key global-map [remap bookmark-jump] 'helm-filtered-bookmarks)
(define-key global-map (kbd "C-:") 'helm-eval-expression-with-eldoc)
(define-key global-map (kbd "C-,") 'helm-calcul-expression)
(define-key global-map (kbd "C-h d") 'helm-info-at-point)
(define-key global-map (kbd "C-h i") 'helm-info)
(define-key global-map (kbd "C-x C-d") 'helm-browse-project)
(define-key global-map (kbd "<f1>") 'helm-resume)
(define-key global-map (kbd "C-h C-f") 'helm-apropos)
(define-key global-map (kbd "C-h a") 'helm-apropos)
(define-key global-map (kbd "C-h C-d") 'helm-debug-open-last-log)
(define-key global-map (kbd "<f6> s") 'helm-find)
(define-key global-map (kbd "S-<f3>") 'helm-execute-kmacro)
(define-key global-map (kbd "C-c i") 'helm-imenu-in-all-buffers)
(define-key global-map (kbd "C-c C-i") 'helm-imenu)
(define-key global-map (kbd "<f11>") 'helm-org-agenda-files-headings)
(define-key global-map (kbd "M-s") 'helm-occur-visible-buffers)
(define-key global-map (kbd "<f6> h") 'helm-emms)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap jump-to-register] 'helm-register)
(define-key global-map [remap list-buffers] 'helm-mini)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap find-tag] 'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
(define-key global-map (kbd "C-x r p") 'helm-projects-history)
(define-key global-map (kbd "C-x r c") 'helm-addressbook-bookmarks)
(define-key global-map (kbd "C-c t r") 'helm-dictionary)

;; Indent or complete with completion-at-point
;; (setq tab-always-indent 'complete)

;; (define-key global-map (kbd "<backtab>") 'completion-at-point)

;; Avoid hitting forbidden directories when using find.
(add-to-list 'completion-ignored-extensions ".gvfs/")
(add-to-list 'completion-ignored-extensions ".dbus/")
(add-to-list 'completion-ignored-extensions "dconf/")

;;; config/60_helm.el ends here
