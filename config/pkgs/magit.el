
;;; magit.el --- magit Configuration

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defun user:switch-to-magit-process-buffer ()
  (interactive)
  (let ((buffer (get-buffer "*magit-process*")))
    (if buffer
      (switch-to-buffer-other-window buffer)
      (error "Buffer *magit-process* not found"))))

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m b") 'magit-blame-mode)
(global-set-key (kbd "C-c m p") 'user:switch-to-magit-process-buffer)

;;; magit.el ends here
