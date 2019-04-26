;;; init.el --- Base installation file

;; Copyright (C) 2012-2019 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(dolist (config-file
         (directory-files "~/.emacs.d/config" t "^[^.].+\\.el$" nil))
  (load config-file))

;;; init.el ends here
