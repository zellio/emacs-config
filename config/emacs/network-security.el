;;; config/emacs/network-security.el ---

;; Copyright (C) 2016 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Initialize global network-security environment values

;;; Code:

(setq
 nsm-settings-file
 (expand-file-name "network-security.data" user/emacs-data-directory))

;;; config/emacs/network-security.el ends here
