;;; package.el --- Setting marmalade extentions to ELPA

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'package)

(setq
 package-user-dir (expand-file-name "elpa" user:emacs-data-directory)
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("marmalade" . "http://marmalade-repo.org/packages/")
                    ("melpa" . "http://melpa.milkbox.net/packages/")))

(defcustom user:package-installed-list-file
  (expand-file-name "package.list" package-user-dir)
  "")

(defun user:package-load-installed-list ()
  ""
  (with-temp-buffer
    (insert-file-contents user:package-installed-list-file)
    (let ((pkg-list (read (current-buffer))))
      (if (null pkg-list) '() pkg-list))))

(defun user:package-save-installed-list ()
  (with-temp-buffer
    (insert "(") (newline)
    (dolist (pkg (sort user:package-installed-list 'string<))
      (insert (symbol-name pkg)) (newline))
    (insert ")") (newline)
    (write-file user:package-installed-list-file nil)))

(defcustom user:package-installed-list
  (user:package-load-installed-list)
  "")

(package-initialize)

(defadvice package-install (after package-save-name (pkg) activate compile)
  ""
  (add-to-list 'user:package-installed-list pkg)
  (user:package-save-installed-list))

(defadvice package-delete (after package-rm-name (pkg &rest a) activate compile)
  ""
  (let ((psym (intern pkg)))
    (setq user:package-installed-list (delete psym user:package-installed-list))
    (user:package-save-installed-list)))

(unless (file-exists-p (expand-file-name "archives" package-user-dir))
  (package-refresh-contents))

(dolist (pkg user:package-installed-list)
  (unless (package-installed-p pkg)
    (when (y-or-n-p (format "Package `%s' is missing. Install it now? " pkg))
      (package-install pkg))))

;; end of package.el
