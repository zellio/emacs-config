
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
(dolist (hash '(  ;; Birds of paradise
  "8281168b824a806489ca7d22e60bb15020bf6eecd64c25088c85b3fd806fc341"
  ;; Solarized Dark
  "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6"
  ;; Solarized Light
  "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365"
  ;; Zenburn
  "0bac11bd6a3866c6dee5204f76908ec3bdef1e52f3c247d5ceca82860cccfa9d"))
  (add-to-list 'custom-safe-themes hash))


(load-theme 'wombat)

;; end of theme.el
