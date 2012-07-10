;; installer script
;; mostly ignore this for now

(print "Generating directories ...")

(dolist (dir '("ac-data" "autosave" "backup" "org" "recovery"))
  (make-directory dir))

(print "Populating vendor git modules")

(shell-command "git submodule update --init --recursive")
(make-symbolic-link "vendor/org-mode.full/lisp" "vendor/org-mode")
