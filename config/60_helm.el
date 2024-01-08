;;; 60_helm.el --- global configurations -*- lexical-binding: t -*-

;; Copyright (C) 2012-2024 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(use-package helm)

(use-package helm-lib
  :ensure nil
  :config
  (helm-help-define-key "C-x" 'exchange-point-and-mark)
  (helm-help-define-key "C-l" 'recenter-top-bottom)
  (helm-help-define-key "C-s" nil)
  (helm-help-define-key "C-r" nil)
  (helm-help-define-key "C-s" 'isl-search)

  (setq
   helm-scroll-amount 4
   helm-find-function-default-project
   '("~/repos/TrialSpark/" "~/repos/zellio/")))

(use-package helm-multi-match
  :ensure nil
  :config
  (setq
   helm-mm-matching-method 'multi3))

(use-package helm-core
  :ensure nil
  :config
  (setq
   helm-completion-window-scroll-margin 12
   helm-candidate-number-limit 512
   helm-input-idle-delay 0.01
   helm-full-frame t
   helm-reuse-last-window-split-state t
   helm-split-window-inside-p t
   helm-display-buffer-width 80
   helm-display-buffer-height 12
   helm-echo-input-in-header-line t
   helm-commands-using-frame nil
   helm-use-frame-when-more-than-two-windows t
   helm-visible-mark-prefix "✓"))

(use-package helm-utils
  :ensure nil
  :hook (find-file . helm-save-current-pos-to-mark-ring)
  :config
  (helm-popup-tip-mode 1)

  (setq
   helm-highlight-matches-around-point-max-lines '(30 . 30)
   helm-window-show-buffers-function #'helm-window-mosaic-fn))

(use-package helm-tags
  :ensure nil
  :config
  (setq
   helm-etags-fuzzy-match t))

(use-package helm-buffers
  :ensure nil
  :general (helm-buffer-map
            "C-d" 'helm-buffer-run-kill-persistent)
  :config
  (setq
   helm-buffers-fuzzy-matching t
   helm-buffer-skip-remote-checking t
   helm-buffer-max-length 22
   helm-buffers-end-truncated-string "…"
   helm-buffers-maybe-switch-to-tab t
   helm-buffers-show-icons t
   helm-mini-default-sources '(helm-source-buffers-list
                               helm-source-buffer-not-found)
   helm-buffers-favorite-modes
   (append helm-buffers-favorite-modes '(picture-mode artist-mode))
   helm-boring-buffer-regexp-list
   '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\*Magit" "\\`\\*git-gutter" "\\`\\*Help")))

(use-package helm-files
  :ensure nil
  :general
  (helm-read-file-map
   "RET" 'helm-ff-RET)
  (helm-find-files-map
   "C-d" 'helm-ff-persistent-delete
   "C-:" 'helm-ff-tramp-methods-complete
   "C-c y" 'helm/insert-date-in-minibuffer)

  :config
  (setq
   helm-ff-auto-update-initial-value t
   helm-ff-allow-non-existing-file-at-point t
   helm-trash-remote-files t
   helm-ff-file-compressed-list '("tgz" "gz" "bz2" "zip" "7z" "rar")
   helm-dwim-target 'next-window
   helm-locate-recursive-dirs-command "fd --hidden --type d --glob '*%s*' %s"

   helm-ff-eshell-unwanted-aliases
   '("sudo" "cdu" "man" "gpg-pubkey-export-armor" "gpg-secretkey-export-armor")

   helm-ff-file-name-history-use-recentf t)

  (customize-set-variable 'helm-ff-lynx-style-map t)

  (defun helm/insert-date-in-minibuffer ()
    (interactive)
    (with-selected-window (or (active-minibuffer-window)
                              (minibuffer-window))
      (unless (or (helm-follow-mode-p)
                  helm--temp-follow-flag)
        (goto-char (point-max))
        (insert (format-time-string "%Y-%m-%d-%H:%M"))))))

(use-package helm-misc
  :ensure nil
  :config
  (setq
   helm-time-zone-home-location "America/New_York")
  (customize-set-variable
   'helm-minibuffer-history-key [remap next-matching-history-element]))

(use-package helm-mode
  :ensure nil
  :diminish (helm-mode " ⎈")
  :general
  (ctl-x-5-map
   "C-x c t" 'helm-top-in-frame
   "C-x c i" 'helm-imenu-in-frame
   "C-x c f" 'helm-find-files-in-frame
   "C-x C-f" 'helm-find-files-in-frame
   "M-x" 'helm-M-x-in-frame
   "C-s" 'helm-occur-in-frame
   "C-x C-b" 'helm-mini-in-frame)

  (helm-command-map
   "z" 'helm-complex-command-history
   "#" 'helm-emms
   "I" 'helm-imenu-in-all-buffers
   "@" 'helm-list-elisp-packages-no-fetch)

  (global-map
   [remap execute-extended-command] 'helm-M-x
   "M-y" 'helm-show-kill-ring
   [remap find-file] 'helm-find-files
   "C-c <SPC>" 'helm-all-mark-rings
   [remap bookmark-jump] 'helm-filtered-bookmarks
   "C-:" 'helm-eval-expression-with-eldoc
   "C-," 'helm-calcul-expression
   "C-h d" 'helm-info-at-point
   "C-h i" 'helm-info
   "C-x C-d" 'helm-browse-project
   "<f1>" 'helm-resume
   "C-h C-f" 'helm-apropos
   "C-h a" 'helm-apropos
   "C-h C-d" 'helm-debug-open-last-log
   "<f6> s" 'helm-find
   "S-<f3>" 'helm-execute-kmacro
   "C-c i" 'helm-imenu-in-all-buffers
   "C-c C-i" 'helm-imenu
   "<f11>" 'helm-org-agenda-files-headings
   "M-s" 'helm-occur-visible-buffers
   "<f6> h" 'helm-emms
   [remap occur] 'helm-occur
   [remap jump-to-register] 'helm-register
   [remap switch-to-buffer] 'helm-buffers-list
   [remap list-buffers] 'helm-mini
   [remap dabbrev-expand] 'helm-dabbrev
   [remap find-tag] 'helm-etags-select
   [remap xref-find-definitions] 'helm-etags-select
   "C-x r p" 'helm-projects-history
   "C-x r c" 'helm-addressbook-bookmarks
   "C-c t r" 'helm-dictionary
   "<backtab>" 'completion-at-point)

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
     (write-region . helm-read-file-name-handler-1))

   tab-always-indent 'complete)

  ;; Avoid hitting forbidden directories when using find.
  (add-to-list 'completion-ignored-extensions ".gvfs/")
  (add-to-list 'completion-ignored-extensions ".dbus/")
  (add-to-list 'completion-ignored-extensions "dconf/")

  ;; Fix CAP with LSP in python.
  (add-to-list 'helm-completion-styles-alist '(python-mode . helm-fuzzy)))

(use-package helm-adaptive
  :ensure nil
  :config
  (setq
   helm-adaptive-history-file nil)
  (helm-adaptive-mode 1))

(use-package helm-sys
  :ensure nil
  :commands (helm-top)
  :config (helm-top-poll-mode 1))

(use-package helm-info
  :ensure nil
  :general ("C-h r" 'helm-info-emacs))

(use-package helm-descbinds
  :config (helm-descbinds-mode 1))

(use-package helm-occur
  :ensure nil
  :general (helm-occur-map "C-M-a" 'helm/occur-which-func)
  :hook (helm-occur-mode . hl-line-mode)
  :config
  (setq helm-occur-keep-closest-position t))

(use-package helm-locate
  :ensure nil
  :config
  (setq
   helm-locate-fuzzy-match t))

(use-package helm-org
  :config
  (setq
   helm-org-headings-fontify t))

(use-package helm-emms
  :after (emms)
  :config
  (setq
   helm-emms-use-track-description-function nil))

(use-package helm-find
  :ensure nil
  :config
  (setq
   helm-find-noerrors t))

(use-package helm-imenu
  :ensure nil
  :config
  (add-to-list 'helm-imenu-type-faces '("^Use package$" . font-lock-keyword-face))
  (customize-set-variable 'helm-imenu-lynx-style-map t))

(use-package helm-epa
  :ensure nil
  :config (helm-epa-mode 1))

(use-package helm-all-the-icons
  :ensure nil
  :commands helm-all-the-icons)

(use-package helm-ls-git
  :config
  (setq helm-ls-git-delete-branch-on-remote t))

(use-package helm-tramp
  :general ("C-c s" 'helm-tramp)
  :config
  (setq
   helm-tramp-control-master t
   helm-tramp-control-master-path "~/.ssh/socket"
   helm-tramp-control-master-prefix ""))

(use-package helm-company
  :after (company)
  :general
  (:keymaps
   '(global-map company-mode-map company-active-map)
   "C-/" 'helm-company))

;;; 60_helm.el ends here
