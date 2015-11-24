
;;; magit.el --- magit Configuration

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m b") 'magit-blame)
(global-set-key (kbd "C-c m p") 'magit-process-mode)

;; Before running Git, Magit by default reverts all unmodified
;; buffers that visit files tracked in the current repository.
;; This can potentially lead to data loss, so you might want to
;; disable this by adding the following line to your init file:

;; (setq magit-auto-revert-mode nil)

(setq magit-last-seen-setup-instructions "1.4.0")

;;; magit.el ends here
