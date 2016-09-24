(add-to-list 'load-path "~/.emacs.d/config")

(load "emacs/env")
(load "emacs/global")
(load "emacs/bindings")
(load "emacs/defuns")
(load "emacs/utf-8")
(load "emacs/frames")
(load "emacs/tabs")
(load "emacs/scratch")
(load "emacs/save-place")
(load "emacs/disabled")
(load "emacs/tramp")
(load "emacs/bookmark")
(load "emacs/url")

(load "pkg/package")
(load "pkg/ido")
(load "pkg/flyspell")
(load "pkg/uniquify")
(load "pkg/eshell")

(load "pkg/auto-complete")
(load "pkg/enh-ruby")
(load "pkg/js2")
(load "pkg/perl")
(load "pkg/org")
(load "pkg/magit")
(load "pkg/markdown")
(load "pkg/notmuch")

(load "emacs/theme")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("427fed191e7a766152e59ef0e2904283f436dbbe259b9ccc04989f3acde50a55" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:foreground "green" :weight bold))))
 '(cperl-hash-face ((t (:foreground "purple" :weight bold :slant italic)))))
