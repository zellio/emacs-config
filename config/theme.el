
;;; theme.el --- theme loading for emacs

;; Copyright (C) 2012,2013 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(unless (boundp 'custom-theme-load-path)
  (defvaralias 'custom-theme-load-path 'load-path))

(unless (boundp 'custom-safe-themes)
  (setq custom-safe-themes '()))

(let ((theme-dir "~/.emacs.d/theme"))
  (let ((themes (directory-files theme-dir)))
    (dolist (theme themes)
      (unless (string= "." (substring theme 0 1))
        (add-to-list 'custom-theme-load-path (concat theme-dir "/" theme))))))


;; SHA256 for "safe" theme load
(dolist (hash '(
  ;; birds-of-paradise-plus-theme.el
  "8281168b824a806489ca7d22e60bb15020bf6eecd64c25088c85b3fd806fc341"

  ;; color-theme-solarized.el
  "1a268a25c38c13b33e527c79a9fea89e6e0dde44dec24e1f23a4ec813ed5a288"

  ;; solarized-dark-theme.el
  "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6"

  ;; solarized-light-theme.el
  "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365"

  ;; wombat-theme.el
  "bbe0b1af952694c3b9ba146c073adcc19c11f6b1c06d1bc34f983b31591628db"

  ;; zenburn-theme.el
  "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29"))
	(add-to-list 'custom-safe-themes hash))

(load-theme 'wombat)

;; end of theme.el
