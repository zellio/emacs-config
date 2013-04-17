
;;; scratch.el --- Scratch buffer Configuration

;; Copyright (C) 2012,2013 Zachary Elliott
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
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  (set-buffer (get-buffer-create "*scratch*"))
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  (set-buffer (get-buffer-create "*scratch*"))
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  nil)

;; end of scratch.el
