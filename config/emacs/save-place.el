
;;; save-place.el --- save-place config

;; Copyright (C) 2012-2014 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'saveplace)

(setq-default save-place t)

(setq save-place-file
      (expand-file-name "saved-places" user:emacs-data-directory))

;; end of save-place.el
