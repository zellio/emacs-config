
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendor/")

(load "~/.emacs.d/config/custom.el" 'noerror)


(load "config/env")
(load "config/global")
(load "config/bindings")
(load "config/defuns")

(load "config/utf-8")
(load "config/frames")
(load "config/tabs")
(load "config/scratch")

(load "config/ido")
(load "config/flyspell")

(load "config/erlang")
(load "config/perl")

(vendor 'auctex 'auctex 'preview-latex)
(vendor 'auto-complete 'auto-complete-config)
(vendor 'bnf-mode)
(vendor 'clojure-mode)
(vendor 'haskell-mode)
(vendor 'javascirpt 'js2-mode)
(vendor 'lua-mode)
(vendor 'magit)
(vendor 'markdown-mode)
(vendor 'org-mode 'org-install)
(vendor 'rainbow-delimiters)
(vendor 'scala-mode)
(vendor 'yaml-mode 'yaml-mode)
