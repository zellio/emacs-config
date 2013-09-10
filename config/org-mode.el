
;;; org-mode.el --- org-mode Configuration

;; Copyright (C) 2012,2013 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

;; Patch for vendor breaking autoloading new org mode
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode.full/lisp")

(require 'org-loaddefs)

;; patch for current version of org as org-agenda-filter is depricaited
;; but sill used
(setq org-agenda-filter 'org-agenda-tag-filter)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-M-RET-may-split-line nil)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-iswitchb)

(setq org-log-done t)

(setq org-agenda-files
      (append
       '("~/.emacs.d/org/work.org")
       (delq nil
             (mapcar
              (lambda (s)
                (if (string= "." (substring s 0 1)) nil
                  (concat "~/projects/" s "/" s ".org")))
              (directory-files "~/projects")))))


(setq org-default-notes-file (expand-file-name "~/.emacs.d/org/notes.org"))
(define-key global-map "\C-cr" 'org-capture)

(setq org-directory "~/.emacs.d/org")
(setq org-mobile-directory "~/.emacs.d/org")
(setq org-mobile-inbox-for-pull "~/.emacs.d/org/mobile.org")

(setq org-todo-keywords
      '((sequence "TODO" "INPR" "WAIT" "DONE")))

(setq org-todo-keyword-faces
      '(("INPR"  . (:foreground "steelblue" :weight bold))
        ("WAIT"  . (:foreground "goldenrod" :weight bold))
        ("PROJECT"  . (:foreground "steelblue" :weight bold))
        ("MAYBE"  . (:foreground "dimgrey" :weight bold)) ))

(setq org-use-speed-commands t)

(add-hook
 'org-mode-hook
 (lambda ()
   (local-set-key [(control return)] 'org-insert-heading-after-current)))

;; end of org-mode.el
