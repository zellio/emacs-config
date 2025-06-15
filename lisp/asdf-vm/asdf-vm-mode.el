;;; asdf-vm-mode.el --- ASDF VM porceline for Emacs -*- lexical-binding: t -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config/main/blob/lisp/asdf-vm-mode
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

(require 'asdf-vm-core)
(require 'asdf-vm-installer)
(require 'asdf-vm-plugin)

(defcustom asdf-vm-mode-line-format "(A)"
  "How `asdf-vm-mode' will indicate activity in the mode line."
  :group 'asdf-vm
  :type 'sexpr)

(defvar asdf-vm-core-command-map
  (let* ((map (make-sparse-keymap)))
    (define-key map "c" #'asdf-vm-current)
    (define-key map "g" #'asdf-vm-global)
    (define-key map "h" #'asdf-vm-help)
    (define-key map "i" #'asdf-vm-install)
    (define-key map "a" #'asdf-vm-latest)
    (define-key map "A" #'asdf-vm-latest-all)
    (define-key map "l" #'asdf-vm-list)
    (define-key map "L" #'asdf-vm-list-all)
    (define-key map "o" #'asdf-vm-local)
    (define-key map "s" #'asdf-vm-shell)
    (define-key map "u" #'asdf-vm-uninstall)
    (define-key map "w" #'asdf-vm-where)
    (define-key map "W" #'asdf-vm-which)
    (define-key map "n" #'asdf-vm-info)
    (define-key map "v" #'asdf-vm-version)
    (define-key map "r" #'asdf-vm-reshim)
    (define-key map "S" #'asdf-vm-shim-versions)
    map)
  "Command keymap for asdf-vm-core.")

(defvar asdf-vm-plugin-command-map
  (let* ((map (make-sparse-keymap)))
    (define-key map "a" #'asdf-vm-plugin-add)
    (define-key map "l" #'asdf-vm-plugin-list)
    (define-key map "L" #'asdf-vm-plugin-list-all)
    (define-key map "r" #'asdf-vm-plugin-remove)
    (define-key map "u" #'asdf-vm-plugin-update)
    (define-key map "U" #'asdf-vm-plugin-update-all)
    map)
  "Command keymap for asdf-vm-plugin.")

(defvar asdf-vm-installer-command-map
  (let* ((map (make-sparse-keymap)))
    (define-key map "i" #'asdf-vm-installer-install)
    (define-key map "u" #'asdf-vm-installer-update)
    map)
  "Command keymap for asdf-vm-installer.")

(defcustom asdf-vm-mode-keymap-prefix "C-c a"
  "Keymode map prefix for `asdf-vm-mode'."
  :group 'asdf-vm-mode
  :type 'string)

(defvar asdf-vm-mode-map
  (when asdf-vm-mode-keymap-prefix
    (let* ((map (make-sparse-keymap))
           (core-kbd (kbd asdf-vm-mode-keymap-prefix))
           (plugin-kbd (kbd (concat asdf-vm-mode-keymap-prefix "P")))
           (installer-kbd (kbd (concat asdf-vm-mode-keymap-prefix "I"))))
      (define-key map core-kbd asdf-vm-core-command-map)
      (define-key map plugin-kbd asdf-vm-plugin-command-map)
      (define-key map installer-kbd asdf-vm-installer-command-map)
      map))
  "Keymap for asdf-vm-mode.")

(easy-menu-define asdf-vm-mode-menu asdf-vm-mode-map
  "ASDF-VM menu."
  `("ASDF-VM"
    ("Plugin"
     ["Add" asdf-vm-plugin-add t :help "Add a plugin from the plugin repo OR, add a Git repo as a plugin by specifying the name and repo url"]
     ["List" asdf-vm-plugin-list t :help "List installed plugins. Optionally show git urls and git-ref"]
     ["List All" asdf-vm-plugin-list-all t :help "List plugins registered on asdf-plugins repository with URLs"]
     ["Remove" asdf-vm-plugin-remove t :help "Remove plugin and package versions"]
     ["Update" asdf-vm-plugin-update t :help "Update a plugin to latest commit on default branch or a particular git-ref"]
     ["Update All" asdf-vm-plugin-update-all t :help "Update all plugins to latest commit on default branch"])
    ("Installer"
     ["Install" asdf-vm-installer-install t :help "Install a new version of ASDF-VM in `asdf-vm-dir'."]
     ["Update" asdf-vm-installer-update t :help "Update ASDF-VM in `asdf-vm-dir' to current git-ref."])
    "---"
    ["Current" asdf-vm-current t :help "Display current version set or being used for all packages"]
    ["Global" asdf-vm-global t :help "Set the package global version"]
    ["Help" asdf-vm-help t :help "Output documentation for plugin and tool"]
    ["Install" asdf-vm-install t :help "Install package at specified version"]
    ["Latest" asdf-vm-latest t :help "Show latest stable version of a package"]
    ["List" asdf-vm-list t :help "List installed versions of a package and optionally filter the versions"]
    ["List All" asdf-vm-list-all t :help "List all versions of a package and optionally filter the returned versions"]
    ["Local" asdf-vm-local t :help "Set the package local version"]
    ["Shell" asdf-vm-shell t :help "Set the `ASDF_${PACKGE}_VERSION` environment variable"]
    ["Uninstall" asdf-vm-uninstall t :help "Remove a specific version of a package"]
    ["Where" asdf-vm-where t :help "Display install path for an installed or current version"]
    ["Which" asdf-vm-which t :help "Display the path to an executable"]))

;;;###autoload
(define-minor-mode asdf-vm-mode
  "Minor mode for asdf-vm interaction.

\\{asdf-vm-mode-map}"
  :global t
  :group 'asdf-vm
  :lighter ""
  :keymap asdf-vm-mode-map
  (if asdf-vm-mode
      (progn
        (add-to-list 'mode-line-misc-info asdf-vm-mode-line-format)
        ;; Inject asdf-vm paths to exec-path based on asdf.sh
        (dolist (component '("shims" "bin"))
          (let ((component-path (file-name-as-directory (asdf-vm-expand-file-name component))))
            (if asdf-vm-force-prepend
                (push component-path exec-path)
              (setq exec-path (append exec-path (list component-path))))))
        ;; Update path variable for shell calls
        (setenv "PATH" (string-join exec-path path-separator)))
    ;; Clean up lighter after ourselves
    (setq
     mode-line-misc-info (delete asdf-vm-mode-line-format mode-line-misc-info))
    ;; Remove previously added paths
    ;; NB. This will also delete user paths if they added them
    (dolist (component '("bin" "shims"))
      (let* ((component-path (file-name-as-directory (asdf-vm-expand-file-name component))))
        (setq exec-path (seq-remove (apply-partially #'string-equal component-path) exec-path))))
    (setenv "PATH" (string-join exec-path path-separator))))

;;;###autoload
(defun asdf-vm-mode-enable ()
  "Unconditionally enable `asdf-vm-mode'."
  (asdf-vm-mode +1))

;;;###autoload
(defun asdf-vm-mode-disable ()
  "Unconditionally disable `asdf-vm-mode'."
  (asdf-vm-mode +1))

(provide 'asdf-vm-mode)

;;; asdf-vm-mode.el ends here
