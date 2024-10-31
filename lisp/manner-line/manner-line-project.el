;;; manner-line-project.el --- Mode line management library -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.0") (project))
;; Homepage: https://github.com/zellio/emacs-config
;; Keywords: mode-line

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

;; Personalized mode line

;;; Code:

(require 'project)
(require 'manner-line-face)

;;; Customization Group

(defgroup manner-line-project nil
  "Customization group for `manner-line-project'."
  :group 'manner-line)

(defvar manner-line-project-hook-alist
  '((manner-line-project-segment-update
     find-file-hook
     after-save-hook))
  "Project `manner-line' feature hooks.")

(defvar manner-line-project-masked-symbols nil
  "Project `manner-line' feature values.")

(defvar-local manner-line-project-segment nil
  "Buffer local display cache for `manner-line' project segment.")

(put 'manner-line-project-segment 'risky-local-variable t)

(defun manner-line-project--file-squash-components (path &optional count)
  "Flatten PATH components into COUNT characters."
  (cl-labels ((take (sequence) (seq-take sequence (or count 1))))
    (let* ((components (file-name-split path))
           (squashed-components (seq-map #'take (butlast components)))
           (squashed-path
            (apply #'file-name-concat (append squashed-components (last components)))))
      (if (file-name-absolute-p path)
          (concat (file-truename "/") squashed-path)
        squashed-path))))

;;;###autoload
(defun manner-line-project-segment-update ()
  "Updater hook function for project mode-line segement."
  (when-let* ((project (project-current nil)) (buffer-file-name (buffer-file-name)))
    (setq
     manner-line-project-segment
     `((:propertize
        ,(project-name project)
        face manner-line-keyword
        mouse-face manner-line-important
        local-map ,(make-mode-line-mouse-map 'mouse-1 #'project-switch-project))
       #(":" 0 1 (face manner-line-unimportant))
       (:propertize
        ,(manner-line-project--file-squash-components
          (file-relative-name
           (file-truename buffer-file-name)
           (project-root project)))
        help-echo ,buffer-file-name)))))

;;; Inject project support to buffer-identification segment

(setq-default
 manner-line-buffer-identification-segment
 `(""
   manner-line-buffer-status-segment
   " "
   (manner-line-project-segment
    manner-line-project-segment
    ,@(propertized-buffer-identification "%b"))))

(put 'manner-line-buffer-spec-segment 'risky-local-variable t)

(provide 'manner-line-project)

;;; manner-line-project.el ends here
