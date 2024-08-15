;;; early-init.el --- initialize compilation and logging -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(setq
 gc-cons-threshold 67108864
 byte-compile-warnings '(not obsolete)
 warning-suppress-log-types '((comp) (bytecomp))
 native-comp-async-report-warnings-errors 'silent
 inhibit-startup-echo-area-message (user-login-name)
 inhibit-startup-screen t
 frame-resize-pixelwise t
 initial-frame-alist '((vertical-scroll-bars . nil))
 package-enable-at-startup nil)

(setq
 gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
 package-user-dir (expand-file-name "vendor" user-emacs-directory)
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                    ("melpa" . "https://melpa.org/packages/"))
 package-pinned-packages '()
 package-native-compile t
 package-install-upgrade-built-in t)

(when (and (native-comp-available-p) (fboundp 'startup-redirect-eln-cache))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

;;; early-init.el ends here
