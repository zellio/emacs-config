;;; package.el --- Setting marmalade extentions to ELPA

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'package)

(setq
 package-user-dir (expand-file-name "elpa" user-emacs-data-directory)
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("marmalade" . "http://marmalade-repo.org/packages/")
                    ("melpa" . "http://melpa.milkbox.net/packages/")))

(defcustom package--user-package-list-file
  (expand-file-name "pkg.list" package-user-dir)
  ""
  :type 'string
  :group 'package
  :version "24.x")

(defun package--user-load-package-list ()
  ""
  (with-temp-buffer
    (insert-file-contents package--user-package-list-file)
    (read (current-buffer))))

(defun package--user-save-package-list ()
  ""
  (with-temp-buffer
    (insert "(")
    (newline)
    (dolist (pkg (sort package--user-package-list 'string<))
      (insert (symbol-name pkg))
      (newline))
    (insert ")")
    (newline)
    (write-file package--user-package-list-file nil)))

(defcustom package--user-package-list
  (package--user-load-package-list)
  ""
  :group 'package)

(package-initialize)

(defadvice package-install (after package-save-name (pkg) activate compile)
  ""
  (add-to-list 'package--user-package-list pkg)
  (package--user-save-package-list))

(defadvice package-delete (after package-rm-name (pkg &rest a) activate compile)
  ""
  (let ((pkg-sym (intern pkg)))
    (setq package--user-package-list (delete pkg-sym package--user-package-list))
    (package--user-save-package-list)))

(unless (file-exists-p (expand-file-name "archives" package-user-dir))
  (package-refresh-contents))

(dolist (pkg package--user-package-list)
  (unless (package-installed-p pkg)
    (when (y-or-n-p (format "Package `%s' is missing. Install it now? " pkg))
      (package-install pkg))))

;; end of package.el
