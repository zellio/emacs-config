;;; asdf-vm-installer.el --- ASDF VM porceline for Emacs -*- lexical-binding: t -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config/main/blob/lisp/asdf-vm-installer
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

(require 'asdf-vm-error)
(require 'asdf-vm-config)
(require 'asdf-vm-process)

(defgroup asdf-vm-installer nil
  "Aasdf-vm installation configuration subgroup."
  :prefix "asdf-vm-installer-"
  :group 'asdf-vm)

(defcustom asdf-vm-installer-git-executable (executable-find "git")
  "Path to git executable used in asdf installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-git-arguments nil
  "Optional extra arguments to be passed to git execution on every call."
  :group 'asdf-vm-installer
  :type '(repeat (string :tag "Git argument")))

(defcustom asdf-vm-installer-git-url "https://github.com/asdf-vm/asdf.git"
  "Source url for asdf-vm installation."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-git-remote "origin"
  "Local remote name for install git repository."
  :group 'asdf-vm-installer
  :type 'string)

(defcustom asdf-vm-installer-git-ref "tags/v0.14.1"
  "Git ref value. This is essentially install version."
  :group 'asdf-vm-installer
  :type 'string)

(defun asdf-vm-installer--git-call (&rest args)
  "Wrapper around `asdf-vm-call' to setup ARGS for git exection.

This function updates the following keys in ARGS when they are missing:

:executable to `asdf-vm-installer-git-executable'

:executable-arguemnts to `asdf-vm-installer-git-arguments'.

:directory to `default-directory'.

:blocking to t."
  (let* ((call-args
          (list
           :executable asdf-vm-installer-git-executable
           :executable-arguments asdf-vm-installer-git-arguments
           :directory default-directory
           :blocking t)))
    (cl-loop for (keyword value) on args by #'cddr do
             (setq call-args (plist-put call-args keyword value)))
    (apply #'asdf-vm-call call-args)))

(defun asdf-vim-installer--git-init ()
  "Initialize the asdf-vm installation directory git repository."
  (asdf-vm-installer--git-call :command 'init))

(defun asdf-vm-installer--git-config ()
  "Configure the asdf-vm installation directory git repository.

This sets up the remote, fetch reference, and disables the detachedHead advice."
  (let* ((remote-prefix (concat "remote." asdf-vm-installer-git-remote "."))
         (remote-url-key (concat remote-prefix "url"))
         (remote-fetch-key (concat remote-prefix "fetch"))
         (fetch-target (format "refs/%s:remotes/%s/%s"
                               asdf-vm-installer-git-ref
                               asdf-vm-installer-git-remote
                               asdf-vm-installer-git-ref)))
    (dolist (arguments `(("advice.detachedHead" "false")
                         (,remote-url-key ,asdf-vm-installer-git-url)
                         (,remote-fetch-key ,fetch-target)))
      (asdf-vm-installer--git-call :command 'config :command-arguments arguments))))

(defun asdf-vm-installer--git-fetch ()
  "Shallow fetch of asdf-vm git repository."
  (asdf-vm-installer--git-call
   :command 'fetch
   :command-arguments '("--depth=1" "--no-tags" "--prune")))

(defun asdf-vm-installer--git-checkout ()
  "Checkout the commit hash from the remote git reference."
  (let* ((target-rev (format "remotes/%s/%s^{commit}" asdf-vm-installer-git-remote asdf-vm-installer-git-ref))
         (response
          (asdf-vm-installer--git-call
           :output t
           :command 'rev-parse
           :command-arguments (list target-rev))))
    (asdf-vm-installer--git-call :command 'checkout :command-arguments (list (string-trim response)))))

;;;###autoload
(defun asdf-vm-installer-install (path)
  "Create, setup, and update a new asdf-vm installation at PATH."
  (interactive
   (list
    (if current-prefix-arg (read-file-name "Installation path: ") asdf-vm-dir)))
  (make-directory path t)
  (let ((default-directory path))
    (asdf-vim-installer--git-init)
    (asdf-vm-installer-update path)))

;;;###autoload
(defun asdf-vm-installer-update (path)
  "Update the asdf-vm installation located at PATH."
  (interactive
   (list
    (if current-prefix-arg (read-file-name "Installation path: ") asdf-vm-dir)))
  (let ((default-directory path))
    (asdf-vm-installer--git-config)
    (asdf-vm-installer--git-fetch)
    (asdf-vm-installer--git-checkout)))

(provide 'asdf-vm-installer)

;;; asdf-vm-installer.el ends here
