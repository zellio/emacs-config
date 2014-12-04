;;; package.el --- Setting marmalade extentions to ELPA

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'package)

(setq
 package-user-dir (expand-file-name "elpa" user:emacs-data-directory)
 package-archives '(("org" . "http://orgmode.org/elpa/")
                    ("gnu" . "http://elpa.gnu.org/packages/")
                    ("marmalade" . "http://marmalade-repo.org/packages/")
                    ("melpa" . "http://melpa.milkbox.net/packages/")))

(defcustom user:package-dist-list-file
  (expand-file-name "dist-packages.list" package-user-dir)
  "")

(defun user:package-load-dist-list ()
  ""
  (with-temp-buffer
    (insert-file-contents user:package-dist-list-file)
    (or (read (current-buffer)) '())))

(defun user:package-save-dist-list ()
  ""
  (with-temp-buffer
    (insert "(")
    (newline)
    (dolist (pkg (sort (mapcar 'car package-alist) 'string<))
      (insert (symbol-name pkg))
      (newline))
    (insert ")")
    (write-file user:package-dist-list-file nil)))

(defun user:package-install-dist-list ()
  ""
  (dolist (pkg (user:package-load-dist-list))
    (unless (package-installed-p pkg)
      (when (y-or-n-p (format "Package `%s' is missing. Install it now? " pkg))
        (package-install pkg)))))

(package-initialize)

(defadvice package-install (after package-save-name (pkg) activate compile)
  ""
  (user:package-save-dist-list))

(defadvice package-delete (after package-rm-name (pkg &rest a) activate compile)
  ""
  (user:package-save-dist-list))

(unless (file-exists-p (expand-file-name "archives" package-user-dir))
  (package-refresh-contents))

(user:package-install-dist-list)

;; end of package.el
