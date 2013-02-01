
;;; defuns.el --- Personal function definitions

;; Copyright (C) 2012 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defmacro filter (fn list)
  `(delq nil (mapcar (lambda (l) (and (funcall ,fn l) l)) ,list)))

;; Taken from https://github.com/rmm5t/dotfiles/blob/master/emacs.d/rmm5t/defuns.el
(defun vendor (library &rest autoload-functions)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/vendor/" file))
         (suffix (concat normal ".el"))
         (personal (concat "~/.emacs.d/config/" file))
         (found nil))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (set 'found t))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (set 'found t))
     ((file-exists-p suffix) (set 'found t)))
    (when found
      (if autoload-functions
          (dolist (autoload-function autoload-functions)
            (autoload autoload-function (symbol-name library) nil t))
        (require library)))
    (when (file-exists-p (concat personal ".el"))
      (load personal))))


;; Quickly jump back and forth between matching parens/brackets
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

;; Make the whole buffer pretty and consistent
(defun indent-whole-buffer()
  "Indent Whole Buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


;; This functionality is currently being rebuilt into themis.el -- more to come

;; This is a very job specific function, will probably be revisited
;; (defun org-agenda-insert-project-remedy-ticket ( inc-id )
;;   (interactive "MIncident ID: ")
;;   (org-agenda-goto)
;;   (let* ((file (buffer-file-name))
;;          (path (file-name-directory file))
;;          (inc-org-file (concat path "docs/remedy/" inc-id ".org")))
;;     (goto-char (point-min))
;;     (search-forward "* Remedy Tickets" (point-max) nil)
;;     (org-insert-todo-heading-respect-content)
;;     (org-do-demote)
;;     (insert "[[" inc-org-file "][" inc-id "]]")
;;     (find-file inc-org-file)
;;     (org-insert-time-stamp nil t t)
;;     (newline)
;;     (save-selected-window
;;       (org-agenda-redo))))

;;; end of defuns.el
