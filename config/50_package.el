;;; config/50_package.el --- setting package extentions

;; Copyright (C) 2012-2020 Zachary Elliott
;; See COPYING for more information






;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(setq
 ;;; Fix bug
 gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"

 ;; This is an ugly hack becuase they won't leave my init file alone.
 package--init-file-ensured t
 ;; We will run '(package-initialize) later in the file
 package-enable-at-startup nil
 package-user-dir (expand-file-name "vendor" user-emacs-directory)
 package-archives '(("org" . "https://orgmode.org/elpa/")
                    ("gnu" . "https://elpa.gnu.org/packages/")
                    ("melpa" . "https://melpa.org/packages/")))

(defcustom user/package-selected-packages-file
  (expand-file-name "selected-packages.list" package-user-dir)
  ""
  :type 'string
  :group 'user)

(defun user/package-save-selected-packages ()
  ""
  (user/serialize
   package-selected-packages user/package-selected-packages-file))

(defun user/package-load-selected-packages ()
  ""
  (user/deserialize user/package-selected-packages-file))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; config/50_package.el ends here
