;;; config/emacs/00_env.el --- set up environment

;; Copyright (C) 2012-2020 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Initialize global environment values

;;; Code:

(defcustom user/emacs-data-directory
  (expand-file-name "data" user-emacs-directory)
  ""
  :type 'string
  :group 'user)

(defcustom user/autosave-directory
  (expand-file-name "auto-save/files/" user/emacs-data-directory)
  ""
  :type 'string
  :group 'user)

(defcustom user/backup-directory
  (expand-file-name "backup" user/emacs-data-directory)
  ""
  :type 'string
  :group 'user)

(defcustom user/recovery-directory
  (expand-file-name "recovery" user/emacs-data-directory)
  ""
  :type 'string
  :group 'user)

(defcustom user/url-configuration-directory
  (expand-file-name "url" user/emacs-data-directory)
  ""
  :type 'string
  :group 'user)

(defcustom user/eshell-directory
  (expand-file-name "eshell" user/emacs-data-directory)
  ""
  :type 'string
  :group 'user)

(defcustom user/nsm-settings-file
  (expand-file-name "network-security.data" user/emacs-data-directory)
  ""
  :type 'string
  :group 'user)

(defcustom user/save-place-file
  (expand-file-name "saved-places" user/emacs-data-directory)
  ""
  :type 'string
  :group 'user)

(defcustom user/bookmark-default-file
  (expand-file-name "bookmarks" user/emacs-data-directory)
  ""
  :type 'string
  :group 'user)

(defcustom user/recentf-save-file
  (expand-file-name "recent-files" user/emacs-data-directory)
  ""
  :type 'string
  :group 'user)

(defmacro user/filter (fn list)
  "Filters LIST by predicate function FN."
  `(delq nil (mapcar (lambda (l) (and (funcall ,fn l) l)) ,list)))

(defun user/match-paren (arg)
  "Jump to matching parenthesis for ARG at point."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

(defun user/indent-whole-buffer ()
  "Indent Whole Buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))
  ;; (untabify (point-min) (point-max)))

(defun user/kill-current-buffer ()
  "Kill the current buffer without prompting the user."
  (interactive)
  (kill-buffer (current-buffer)))

(defun user/revert-all-buffers ()
  "Replace all buffers with their visited file contents on disk."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and
             (buffer-file-name)
             (file-exists-p (buffer-file-name))
             (buffer-modified-p))
        (revert-buffer t t t)
        (message "Reverted buffer %s" buffer)))))

(defun user/serialize (data path)
  "Serialize the s-expr DATA into file at PATH."
  (let ((dir (file-name-directory path)))
    (if (not (file-directory-p dir))
        (mkdir dir t))
    (with-temp-buffer
      (insert
       ";; -*- mode: lisp; coding: utf-8 -*-\n"
       ";; emacs-version: " emacs-version "\n"
       "\n"
       (prin1-to-string data))
      (write-file path nil))
    ))

(defun user/deserialize (file)
  "Deserialize the FILE at PATH into an s-expr."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer)))
    '()))

(defun user/smart-indent ()
  "Indent region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(defun user/kill-scratch-buffer ()
  ""
  (with-current-buffer "*scratch*"
    (delete-region (point-min) (point-max)))
  nil)

;; Construct data directories
(dolist (directory (list user/emacs-data-directory
                         user/autosave-directory))
  (unless (file-directory-p directory)
    (mkdir directory t)))

;;; config/emacs/00_env.el ends here
