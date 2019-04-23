;;; config/60_helm.el --- global configurations

;; Copyright (C) 2012-2019 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'helm-config)

(setq
 helm-split-window-in-side-p t
 helm-move-to-line-cycle-in-source t
 helm-ff-search-library-in-sexp t
 helm-echo-input-in-header-line t
 helm-mode-fuzzy-match t
 helm-completion-in-region-fuzzy-match t)

;;; Global-map
(global-set-key [remap execute-extended-command] 'helm-M-x)
(global-set-key [remap find-file] 'helm-find-files)
(global-set-key [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key [remap find-tag] 'helm-etags-select)
(global-set-key [remap jump-to-register] 'helm-register)
(global-set-key [remap list-buffers] 'helm-buffers-list)
(global-set-key [remap occur] 'helm-occur)
(global-set-key [remap xref-find-definitions] 'helm-etags-select)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c f") 'helm-recentf)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c <SPC>") 'helm-all-mark-rings)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-:") 'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,") 'helm-calcul-expression)
(global-set-key (kbd "C-h i") 'helm-info-at-point)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "<f1>") 'helm-resume)
(global-set-key (kbd "C-h C-f") 'helm-apropos)
(global-set-key (kbd "C-c i") 'helm-imenu-in-all-buffers)

;; (define-key shell-mode-map (kbd "M-p") 'helm-comint-input-ring)

(helm-mode 1)
(helm-autoresize-mode 1)

;;; config/60_helm.el ends here
