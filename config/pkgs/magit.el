
;;; magit.el --- magit Configuration

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m b") 'magit-blame-mode)
(global-set-key (kbd "C-c m p") 'magit-process)

;;; magit.el ends here
