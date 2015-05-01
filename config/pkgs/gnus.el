
;;; gnus.el --- GNUS mail configuration

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'image)
(require 'gnus)

(setq
 gnus-directory (expand-file-name "gnus" user:emacs-data-directory)
 message-directory (expand-file-name "mail" gnus-directory)
 nnml-directory (expand-file-name "nnml-mail" gnus-directory)
 gnus-article-save-directory (expand-file-name "saved" gnus-directory)
 gnus-kill-files-directory (expand-file-name "scores" gnus-directory)
 gnus-cache-directory (expand-file-name "cache" gnus-directory)

 gnus-select-method '(nnimap "zell.io"
                             (nnimap-address "imap.vanhijk.com")
                             (nnimap-server-port 993)
                             (nnimap-stream ssl))


 )

(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "● ")
  (setq gnus-sum-thread-tree-false-root "○ ")
  (setq gnus-sum-thread-tree-single-indent "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))

(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
(setq gnus-summary-display-arrow t)
;; end of gnus.el
