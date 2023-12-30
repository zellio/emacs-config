;;; init.el --- Base installation file -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq
 use-package-check-before-init t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package no-littering
  :ensure t
  :config
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))

   backup-directory-alist
   `(("." . ,(no-littering-expand-var-file-name "backup/")))

   undo-tree-history-directory-alist
   `(("." . ,(no-littering-expand-var-file-name "undo-tree-hist/")))

   custom-file (no-littering-expand-etc-file-name "custom.el")))

(let ((config-files (directory-files (concat user-emacs-directory "config") t "^[^.].+\\.el$" nil))
      (custom-files (list custom-file (no-littering-expand-etc-file-name "site-config.el"))))
  (dolist (config-file (append config-files custom-files) )
    (when (file-readable-p config-file)
      (load config-file))))

;;; init.el ends here
