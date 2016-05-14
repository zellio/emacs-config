;;; config/emacs/env.el --- Set up environment

;; Copyright (C) 2012-2016 Zachary Elliott
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
