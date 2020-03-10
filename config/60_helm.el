;;; config/60_helm.el --- global configurations

;; Copyright (C) 2012-2020 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(use-package helm
  :init (progn
          (helm-mode 1)
          (helm-autoresize-mode 1)

          (require 'helm-config)

          (with-eval-after-load 'shell-mode
            (define-key shell-mode-map (kbd "M-p") 'helm-comint-input-ring)))

  :config (setq
           helm-split-window-in-side-p t
           helm-move-to-line-cycle-in-source t
           helm-ff-search-library-in-sexp t
           helm-echo-input-in-header-line t
           helm-mode-fuzzy-match t
           helm-completion-in-region-fuzzy-match t)

  :bind (([remap execute-extended-command] . helm-M-x)
         ([remap find-file] . helm-find-files)
         ([remap dabbrev-expand] . helm-dabbrev)
         ([remap find-tag] . helm-etags-select)
         ([remap jump-to-register] . helm-register)
         ([remap list-buffers] . helm-buffers-list)
         ([remap occur] . helm-occur)
         ([remap xref-find-definitions] . helm-etags-select)

         ("M-y" . helm-show-kill-ring)
         ("C-c f" . helm-recentf)
         ("C-x b" . helm-mini)
         ("C-c <SPC>" . helm-all-mark-rings)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-h r" . helm-info-emacs)
         ("C-:" . helm-eval-expression-with-eldoc)
         ("C-," . helm-calcul-expression)
         ("C-h i" . helm-info-at-point)
         ("C-x C-d" . helm-browse-project)
         ("<f1>" . helm-resume)
         ("C-h C-f" . helm-apropos)
         ("C-c i" . helm-imenu-in-all-buffers)))

(use-package helm-tramp
  :config (setq
           helm-tramp-control-master t
           helm-tramp-control-master-path "~/.ssh/socket"
           helm-tramp-control-master-prefix "")

  :bind (("C-c s" . helm-tramp)))

(use-package helm-lsp
  ;; Configured in lsp include
  )


;;; config/60_helm.el ends here
