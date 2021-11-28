;;; init.el --- Base installation file

;; Copyright (C) 2012-2020 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(dolist (config-file
         (directory-files "~/.emacs.d/config" t "^[^.].+\\.el$" nil))
  (load config-file))

(dolist (extra-file (list
                     custom-file
                     (no-littering-expand-etc-file-name "site-config.el")))
  (when (file-readable-p extra-file)
    (load extra-file)))

;;; init.el ends here
