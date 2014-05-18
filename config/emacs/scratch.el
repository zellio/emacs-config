
;;; scratch.el --- Scratch buffer Configuration

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq initial-scratch-message "")
(setq initial-major-mode 'markdown-mode)

(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'user:kill-scratch-buffer))

(defun user:kill-scratch-buffer ()
  (set-buffer (get-buffer-create "*scratch*"))
  (remove-hook 'kill-buffer-query-functions 'user:kill-scratch-buffer)
  (kill-buffer (current-buffer))
  (set-buffer (get-buffer-create "*scratch*"))
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'user:kill-scratch-buffer)
  nil)

;; end of scratch.el
