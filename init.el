
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendor/")

(load "~/.emacs.d/config/custom.el" 'noerror)


(load "config/env")
(load "config/global")
(load "config/bindings")
(load "config/defuns")

(load "config/utf-8")
(load "config/frames")
(load "config/theme")
(load "config/tabs")
(load "config/scratch")

(load "config/org-mode")
(load "config/ido")
(load "config/flyspell")
(load "config/uniquify")

(load "config/erlang")
(load "config/perl")
(load "config/ruby")

(load "vendor/git-modes/git-commit-mode")
(load "vendor/git-modes/git-rebase-mode")


(vendor 'auctex 'auctex 'preview-latex)
(vendor 'auto-complete 'auto-complete-config)
(vendor 'bnf-mode)
(vendor 'clojure-mode)
(vendor 'cmake-mode)
(vendor 'guru-mode)
(vendor 'haskell-mode)
(vendor 'javascirpt 'js2-mode)
(vendor 'j-mode)
(vendor 'lua-mode)
(vendor 'magit)
(vendor 'markdown-mode)
;;(vendor 'org-mode)
(vendor 'php-mode)
(vendor 'puppet-mode)
(vendor 'rainbow-delimiters)
(vendor 'scala-mode)
(vendor 'scss-mode)
(vendor 'yaml-mode 'yaml-mode)
