
;;; uniquify.el --- uniquify Configuration

;; Copyright (C) 2012,2013 Zachary Elliott
;; See LICENSE for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(require 'uniquify)

(setq
 ;; If non-nil, rerationalize buffer names after a buffer has been killed.
 ;;
 ;; Defaults to ON
 uniquify-after-kill-buffer-p nil

 ;; If non-nil, permit user to choose names for buffers with same base
 ;; file. Hide If the user chooses to name a buffer, uniquification is preempted
 ;; and no other buffer names are changed.
 ;;
 ;; Defaults to OFF
 uniquify-ask-about-buffer-names-p nil

 ;; If non-nil, buffer names are uniquified with parts of directory name. The
 ;; value determines the buffer name style and is one of `forward', `reverse',
 ;; `post-forward', or `post-forward-angle-brackets'.  For example, files
 ;; `/foo/bar/mumble/name' and `/baz/quux/mumble/name' would have the following
 ;; buffer names in the various styles: forward bar/mumble/name quux/mumble/name
 ;; reverse name\mumble\bar name\mumble\quux post-forward name|bar/mumble
 ;; name|quux/mumble post-forward-angle-brackets name<bar/mumble>
 ;; name<quux/mumble> nil name name<2> Of course, the "mumble" part may be
 ;; stripped as well, depending on the setting of
 ;; `uniquify-strip-common-suffix'.
 ;;
 ;; Defaults to nil
 uniquify-buffer-name-style 'forward

 ;; Regular expression matching buffer names that should not be uniquified. For
 ;; instance, set this to "^draft-[0-9]+$" to avoid having uniquify rename draft
 ;; buffers even if `uniquify-after-kill-buffer-p' is non-nil and the visited
 ;; file name isn't the same as that of the buffer.
 ;;
 ;; Defaults to ""
 uniquify-ignore-buffers-re ""

 ;; Minimum number of directory name components included in buffer name.
 ;;
 ;; Defaults to 0
 uniquify-min-dir-content 0

 ;; String separator for buffer name components. Hide When
 ;; `uniquify-buffer-name-style' is `post-forward', separates base file name from
 ;; directory part in buffer names (default "|").  When
 ;; `uniquify-buffer-name-style' is `reverse', separates all file name components
 ;; (default "\").
 ;;
 ;; Defaults to nil
 uniquify-separator "|"

 ;; If non-nil, strip common directory suffixes of conflicting files.  E.g. if
 ;; you open /a1/b/c/d and /a2/b/c/d, the buffer names will say "d|a1" and "d|a2"
 ;; instead of "d|a1/b/c" and "d|a2/b/c".  This can be handy when you have deep
 ;; parallel hierarchies.
 ;;
 ;; Defaults to ON
 uniquify-strip-common-suffix nil

 ;; If non-nil, add a file name separator to dired buffer names.
 ;; If `uniquify-buffer-name-style' is `forward', add the separator at the end;
 ;; if it is `reverse', add the separator at the beginning; otherwise, this
 ;; variable is ignored.
 ;;
 ;; Defaults to OFF
 uniquify-trailing-separator-p nil
)
