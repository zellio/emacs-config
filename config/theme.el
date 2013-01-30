
;;; theme.el --- theme loading for emacs

;; Copyright (C) 2012 Zachary Elliott
;;
;; Authors: Zachary Elliott <ZacharyElliott1@gmail.com>
;; URL:
;; Version: 1.0.0
;; Keywords:

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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



;;(load-theme 'zenburn)

;; SHA256 for "safe" theme load
;; (add-to-list 'custom-safe-themes
;;              "8281168b824a806489ca7d22e60bb15020bf6eecd64c25088c85b3fd806fc341")
;; (load-theme 'birds-of-paradise-plus)
;;
;; (custom-theme-set-faces
;;  'birds-of-paradise-plus
;;  `(default ((t (:background "#2E3436" :foreground "#E6E1C4"))))
;;  `(fringe ((t (:background "#2A2C2D" :foreground "#654D4D")))))
(load-theme 'wombat)
