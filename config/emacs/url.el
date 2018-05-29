;;; config/emacs/url.el --- url configurations

;; Copyright (C) 2012-2017 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq
 url-configuration-directory
 (expand-file-name "url" user/emacs-data-directory))

(make-directory url-configuration-directory t)

;;; config/emacs/url.el ends here
