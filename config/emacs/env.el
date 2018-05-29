;;; config/emacs/env.el --- set up environment

;; Copyright (C) 2012-2017 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Initialize global environment values

;;; Code:

(defcustom user/emacs-data-directory
  (expand-file-name "data" user-emacs-directory)
  ""
  :type 'string)

;;; config/emacs/env.el ends here
