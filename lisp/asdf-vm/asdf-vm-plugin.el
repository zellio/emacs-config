;;; asdf-vm-plugin.el --- ASDF VM porceline for Emacs -*- lexical-binding: t -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config/main/blob/lisp/asdf-vm-plugin
;; Keywords: languages asdf

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;

;;; Code:

(require 'cl-macs)

(require 'asdf-vm-process)
(require 'asdf-vm-util)

(defgroup asdf-vm-plugin nil
  "Configuration subgroup for ASDF-VM plugins."
  :prefix "asdf-vm-plugin-"
  :group 'asdf-vm)

(defcustom asdf-vm-plugin-repository-path (asdf-vm-expand-file-name "repository")
  "Path to the asdf-vm plugin repository."
  :group 'asdf-vm-plugin
  :type 'string)

(defun asdf-vm-plugin--parse-repository-file (path)
  "Read and extract the repository value from a plugin repository file at PATH."
  (unless (file-readable-p path)
    (signal 'asdf-vm-plugin-unreadable-repository-file (list path)))
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-min))
    (search-forward "repository = ")
    (cons
     (file-name-base path)
     (buffer-substring-no-properties (point) (line-end-position)))))

(defvar asdf-vm-plugin--repository-alist nil
  "Memoization variable for `asdf-vm-plugin--repository-alist' function.")

(defun asdf-vm-plugin--repository-alist ()
  "Alist representation of the on disk plugin repository.

NB. This value gets memoized on first call and may need to be manually
unset if on disk representation changes."
  (or asdf-vm-plugin--repository-alist
      (setq asdf-vm-plugin--repository-alist
            (let* ((plugins-directory (expand-file-name "plugins" asdf-vm-plugin-repository-path))
                   (plugin-repository-paths (directory-files plugins-directory t (rx line-start (not ?.)))))
              (seq-map #'asdf-vm-plugin--parse-repository-file plugin-repository-paths)))))

(defun asdf-vm-plugin--plugin-completing-read (&optional name)
  "Completing read for working with NAME of plugins."
  (or name
      (let* ((plugin-names (seq-map #'car (asdf-vm-plugin--repository-alist))))
        (completing-read "Plugin name: " plugin-names))))

(defun asdf-vm-plugin--git-url-completing-read (name &optional git-url)
  "Completing read for working with GIT-URL of NAME plugin."
  (or git-url
      (alist-get name (asdf-vm-plugin--repository-alist) nil nil #'string-equal)
      (read-string "Plugin git url: ")))

(defun asdf-vm-plugin--plugin-and-git-url-completing-read (&optional name git-url)
  "Completing read for working with both NAME and GIT-URL of plugins."
  (let ((name (asdf-vm-plugin--plugin-completing-read name)))
    (list
     name
     (asdf-vm-plugin--git-url-completing-read name git-url))))

(defun asdf-vm-plugin-add (name git-url &optional interactive-call)
  "Add the plugin NAME from GIT-URL to asdf.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (append
    (asdf-vm-plugin--plugin-and-git-url-completing-read)
    (list (prefix-numeric-value current-prefix-arg))))
  (when interactive-call
    (asdf-vm-message "Installing plugin: %s" name))
  (asdf-vm-call
   :command '(plugin add)
   :command-arguments (list name git-url)
   :blocking interactive-call))

(defun asdf-vm-plugin--installed-plugins ()
  "List plugins installed for asdf based on disk state."
  (let* ((plugins-directory (asdf-vm-data-expand-file-name "plugins"))
         (plugin-paths (directory-files plugins-directory t (rx line-start (not ?.)))))
    (seq-map #'file-name-base plugin-paths)))

(defun asdf-vm-plugin-list (&optional interactive-call)
  "List plugins installed for asdf.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive "p")
  (let ((plugins (asdf-vm-plugin--installed-plugins)))
    (when interactive-call
      (asdf-vm-message "Installed plugins: %s" (string-join plugins ", ")))
    plugins))

(defun asdf-vm-plugin-list-all (&optional interactive-call)
  "List all plugins for asdf in the on disk repository.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive "p")
  (cl-labels ((listify-alist (cons) (list (car cons) (cdr cons))))
    (let* ((plugins (seq-map #'listify-alist (asdf-vm-plugin--repository-alist))))
      (when interactive-call
        (asdf-vm-message "Available plugins: %s" (string-join (seq-map #'car plugins) ", ")))
      plugins)))

(defun asdf-vm-plugin--installed-plugin-completing-read (&optional require-match)
  "Completing read for installed asdf plugins.

REQUIRE-MATCH value is as in `completing-read'."
  (let* ((plugins (asdf-vm-plugin--installed-plugins)))
    (completing-read "Installed plugin name: " plugins nil require-match)))

(defun asdf-vm-plugin-remove (name &optional interactive-call)
  "Remove plugin NAME from asdf.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (asdf-vm-plugin--installed-plugin-completing-read)
    (prefix-numeric-value current-prefix-arg)))
  (when interactive-call
    (asdf-vm-message "Removing plugin: %s" name))
  (asdf-vm-call
   :command '(plugin remove)
   :command-arguments (list name)
   :blocking interactive-call))

(defun asdf-vm-plugin-update (name &optional git-ref interactive-call)
  "Update plugin NAME to optional GIT-REF or current GIT-REF.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive
   (list
    (asdf-vm-plugin--installed-plugin-completing-read t)
    (read-string "Plugin git ref: ")
    (prefix-numeric-value current-prefix-arg)))
  (let ((command-arguments (if git-ref (list name git-ref) (list name))))
    (when interactive-call
      (asdf-vm-message "Updating plugin: %s" name))
    (asdf-vm-call
     :command '(plugin update)
     :command-arguments command-arguments
     :blocking interactive-call)))

(defun asdf-vm-plugin-update-all (&optional interactive-call)
  "Update all asdf plugins based on current git-ref values.

INTERACTIVE-CALL is an internal flag value and should not be used manually."
  (interactive "p")
  (when interactive-call
    (asdf-vm-message "Updating all installed plugins"))
  (asdf-vm-call
   :command '(plugin update)
   :command-arguments '("--all")
   :blocking interactive-call))

(provide 'asdf-vm-plugin)

;;; asdf-vm-plugin.el ends here
