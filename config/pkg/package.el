;;; config/pkg/package.el --- setting package extentions

;; Copyright (C) 2012-2016 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'package)

(setq
 ;; This is an ugly hack becuase they won't leave my init file alone.
 package--init-file-ensured t
 ;; We will run '(package-initialize) later in the file
 package-enable-at-startup nil
 package-user-dir (expand-file-name "vendor" user-emacs-directory)
 package-archives '(("org" . "http://orgmode.org/elpa/")
                    ("gnu" . "http://elpa.gnu.org/packages/")
                    ("marmalade" . "http://marmalade-repo.org/packages/")
                    ("melpa" . "http://melpa.milkbox.net/packages/")))

(defcustom user/package-selected-packages-file
  (expand-file-name "selected-packages.list" package-user-dir)
  "")

(defun user/package-save-selected-packages ()
  ""
  (user/serialize
   package-selected-packages user/package-selected-packages-file))

(defun user/package-load-selected-packages ()
  ""
  (user/deserialize user/package-selected-packages-file))

(package-initialize)

(setq
 package-selected-packages
 (user/deserialize user/package-selected-packages-file))

(unless (file-exists-p (expand-file-name "archives" package-user-dir))
  (package-refresh-contents))

(package-install-selected-packages)

(defadvice package--update-selected-packages
    (after user/advice-package--update-selected-packages activate compile)
  ""
  (user/package-save-selected-packages))

;;; config/pkg/package.el ends here