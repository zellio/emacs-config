;;; config/pkg/yaml.el --- yaml-mode configuration

;; Copyright (C) 2016 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

;;(require 'ansible-vault)

(add-to-list 'auto-mode-alist '("/encrypted$" . yaml-mode))

(add-hook 'yaml-mode-hook
          (lambda ()
            (and (ansible-vault--is-vault-file) (ansible-vault-mode 1))))

;;; config/pkg/yaml.el ends here
