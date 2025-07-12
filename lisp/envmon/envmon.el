;;; envmon.el --- Environment magic for Emacs -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.2.0
;; Package-Requires: ((emacse "30.0") (cl-macs))
;; Homepage: http://github.com/zellio/emacs-config/blob/main/lisp/envmon
;; Keywords: environment, linux, shell

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-macs)

(defgroup envmon nil
  "Configuration group for `envmon'."
  :prefix "envmon-"
  :group 'environment)

(defcustom envmon-mode-lighter " (E)"
  "Minor mode lighter for `envmon'."
  :group 'envmon
  :type 'string)

(put 'envmon-mode-lighter 'risky-local-variable t)

;;; Environment Management

(defcustom envmon-environment-allow-list nil
  "List of environment variables to fetch from shell."
  :group 'env-from-shell-environment
  :type '(choice
          (repeat (string :tag "Environment Variable Name"))
          (const :tag "Allow all environment variables" all)))

(defcustom envmon-environment-deny-list nil
  "List of environment variables ignore from shell."
  :group 'env-from-shell-environment
  :type '(choice
          (repeat (string :tag "Environment Variable Name"))
          (const :tag "Do not ignore any variables" nil)))

(defun envmon--message (format-string &rest args)
  "Wrapper for `message' function.

FORMAT-STRING and ARGS are the same as `message'."
  (apply #'message (concat "[envmon] " format-string) args))

(define-error 'envmon-error "[envmon] Base error" '(error))

(defsubst envmon--get-environment-cons (name)
  "Cons cell of NAME and `getenv' of NAME."
  (cons name (getenv name)))

(defsubst envmon--get-environment-alist (names)
  "Maps `envmon--get-environment-cons' over NAMES."
  (seq-map #'envmon--get-environment-cons names))

(defun envmon--set-environment-alist (environment-alist)
  "Call `setenv' for all environment-cons cells in ENVIRONMENT-ALIST."
  (pcase-dolist (`(,name . ,value) environment-alist)
    (setenv name value)))

;;;###autoload
(defmacro envmon-with-environment-alist (environment-alist &rest body)
  "Run BODY with environment from ENVIRONMENT-ALIST.

NB. soft reimplementation of `with-environment-variables'."
  `(let* ((process-environment (copy-sequence process-environment)))
     ,@(seq-map
        (lambda (environment-cons)
          `(setenv ,(car environment-cons) ,(cdr environment-cons)))
        environment-alist)
     ,@body))

(defun envmon--format-environment-cons (environment-cons)
  "Formats ENVIRONMENT-CONS into `car'=`cdr'."
  (pcase-let* ((`(,name . ,value) environment-cons))
    (if (null value) name
      (format "%s=%s" name value))))

(defun envmon--parse-environment-cons (line)
  "Split a LINE on = into an environment-cons cell."
  (let* ((index (seq-position line ?= #'char-equal))
         (name (substring-no-properties line 0 index)))
    (cons name (and index (substring-no-properties line (+ index 1))))))

(defun envmon--format-environment-alist (environment-alist &optional separator)
  "Format ENVIRONMENT-ALIST and join on SEPARATOR."
  (string-join
   (seq-map #'envmon--format-environment-cons environment-alist)
   (or separator "\n")))

(defun envmon--allow-filter-environment-alist (environment-alist &optional allow-list)
  "Fillter ENVIRONMENT-ALIST for only allowed keys from ALLOW-LIST.

NB. When ALLOW-LIST is nil or the symbol `all' the filter will allow all
values through."
  (let* ((allow-list (or allow-list envmon-environment-allow-list)))
    (if (or (null allow-list) (eq allow-list 'all))
        environment-alist
      (seq-map (lambda (name) (assoc name environment-alist)) allow-list))))

(defun envmon--deny-filter-environment-alist (environment-alist &optional deny-list)
  "Filter ENVIRONMENT-ALIST, removing values from DENY-LIST."
  (let* ((deny-list (cons "_" (or deny-list envmon-environment-deny-list))))
    (seq-filter (lambda (environment-cons)
                  (not (member (car environment-cons) deny-list)))
                environment-alist)))

(defun envmon--filter-environment-alist (environment-alist &optional allow-list deny-list)
  "Apply ALLOW-LIST and DENY-LIST filters to ENVIRONMENT-ALIST."
  (envmon--deny-filter-environment-alist
   (envmon--allow-filter-environment-alist environment-alist allow-list) deny-list))

;;; Symbol Management

(defvar envmon--symbol-environment-alist
  '((data-directory . ("EMACSDATA"))
    (doc-directory . ("EMACSDOC"))
    (load-path . ("EMACSLOADPATH"))
    (exec-path . ("EMACSPATH" "PATH"))
    (user-mail-address . ("EMAIL"))
    (user-full-name . ("NAME"))
    (mail-default-reply-to . ("REPLYTO"))
    (shell-file-name . ("ESHELL" "SHELL"))
    (smtpmail-smtp-server . ("SMTPSERVER"))
    (temporary-file-directory . ("TMPDIR" "TMP" "TEMP"))
    (version-control . ("VERSION_CONTROL")))
  "Update dependency mapping from Emacs symbol to environment variable.")

(defun envmon--symbol-update-default (symbol-cons)
  "Default symbol updater for undifferentiated SYMBOL-CONS."
  (pcase-let* ((`(,symbol . ,names) symbol-cons))
    (set symbol (car (seq-keep #'getenv names)))))

(define-error 'envmon-unimplemented "[envmon] Unimplemented method" '(envmon-error))

(defun envmon--symbol-update-load-path (_symbol-cons)
  "Unimplemented _SYMBOL-CONS."
  (signal 'envmon-unimplemented '(envmon--symbol-update-load-path)))

(defsubst envmon--parse-env-colon-path (name)
  "`parse-colon-path' from environment variable NAME."
  (parse-colon-path (getenv name)))

(defun envmon--symbol-update-exec-path (symbol-cons)
  "Update the symbol `exec-path' from the values in SYMBOL-CONS."
  (let* ((paths (seq-mapcat #'envmon--parse-env-colon-path (cdr symbol-cons))))
    (setq exec-path (delete-dups (append paths (list exec-directory))))
    (when (fboundp 'eshell-set-path)
      (eshell-set-path exec-path))))

(define-error
 'envmon-unsuported-symbol
 "[envmon] Attempt to update unsupported symbol"
 '(envmon-error))

(defun envmon--symbol-update (symbol)
  "Delegated SYMBOL update from environment."
  (if-let* ((symbol-cons (assoc symbol envmon--symbol-environment-alist))
            (update-func (intern (format "envmon--symbol-update-%s" symbol))))
      (if (fboundp update-func) (funcall update-func symbol-cons)
        (envmon--symbol-update-default symbol-cons))
    (signal 'envmon-unsupported-symbol (list symbol))))

(defun envmon--set-symbol-alist (symbols-alist)
  "Set all symbol keys in SYMBOLS-ALIST to associated values."
  (pcase-dolist (`(,symbol . ,value) symbols-alist)
    (set symbol value)))

(defun envmon--affected-symbols (environment-alist)
  "Identify symbols affected by ENVIRONMENT-ALIST."
  (cl-labels
      ((affected-symbol-p (symbol-cons)
         (pcase-let* ((`(,symbol . ,names) symbol-cons))
           (and (seq-find (lambda (name) (assoc name environment-alist)) names)
                symbol))))
    (seq-keep #'affected-symbol-p envmon--symbol-environment-alist)))

(defun envmon--update-symbols (environment-alist)
  "Update symbols identified by `envmon--affected-symbols' in ENVIRONMENT-ALIST."
  (let* ((symbols (envmon--affected-symbols environment-alist)))
    (dolist (symbol symbols)
      (envmon--symbol-update symbol))))

;;; Generation Management

(defvar envmon--generation-history-alist nil
  "History storage form generational snapshots.")

(defun envmon--environment-snapshot (environment-alist)
  "Generate snapshot of current environment based on ENVIRONMENT-ALIST."
  (seq-map
   (lambda (environment-cons)
     (let ((name (car environment-cons)))
       (cons name (getenv name))))
   environment-alist))

(defun envmon--symbols-snapshot (environment-alist)
  "Generate snapshot of current symbols based on ENVIRONMENT-ALIST."
  (let ((symbols (envmon--affected-symbols environment-alist)))
    (seq-map (lambda (symbol) (cons symbol (symbol-value symbol))) symbols)))

(defun envmon--generation-snapshot (generation environment-alist)
  "Create new snapshot at time GENERATION based on ENVIRONMENT-ALIST."
  `(,generation
    . ((environment . ,(envmon--environment-snapshot environment-alist))
       (symbols . ,(envmon--symbols-snapshot environment-alist)))))

(defun envmon-snapshot-generation (environment-alist &optional generation)
  "Generate snapshot from ENVIRONMENT-ALIST at GENERATION and save."
  (let* ((generation (or generation (float-time)))
         (snapshot (envmon--generation-snapshot generation environment-alist)))
    (push snapshot envmon--generation-history-alist)))

(define-error
 'envmon-invalid-rollback
 "Invalid rollback attempt, no valid generation found"
 '(envmon-error))

;;;###autoload
(defun envmon-rollback-generation ()
  "Rollback state to youngest generation in history."
  (interactive)
  (if-let* ((generation (pop envmon--generation-history-alist)))
      (let-alist (cdr generation)
        (envmon--set-environment-alist .environment)
        (envmon--set-symbol-alist .symbols))
    (signal 'envmon-invalid-rollback nil)))

;;; Proc

(defcustom envmon-shell-executable nil
  "Source shell from environment variables.

If the value is a path to an executable, use that. If the value is a
string, attempt to locate via `executable-find'. If you want `envmon' to
guess for you set this value to `guess' or nil."
  :group 'envmon
  :type '(choice
          (file :tag "Path to shell executable")
          (string :tag "Name of shell executable on `$PATH'")
          (const :tag "Use shell-file-name value" shell-file-name)
          (const :tag "Attempt to discern proper shell executable." guess)
          (const :tag "Alias for `guess'" nil)))

(defcustom envmon-shell-executable-arguments nil
  "Arguments to pass to `env-from-shell-process-executable'."
  :group 'envmon
  :type '(repeat (string :tag "Shell argument")))

(defvar envmon-shell-executable-default-arguments-alist
  '((bash . ("-l" "-i" "-c"))
    (dash . ("-l" "-i" "-c"))
    (zsh . ("--login" "--interactive" "-c"))
    (fish . ("--login" "--interactive" "--command")))
  "Default shell argument mapping for extracting environment variables.")

(define-error
 'envmon-missing-shell-executable
 "[envmon] Failed to locate executable from `envmon-shell-executable'"
 '(envmon-error))

;;;###autoload
(defun envmon-shell-executable ()
  "Locate shell program from which to fetch environment varialbes."
  (or (pcase envmon-shell-executable
        ((pred stringp) (executable-find envmon-shell-executable))
        ('shell-file-name shell-file-name)
        ((or 'guess (pred null)) (or shell-file-name (getenv "SHELL"))))
      (signal 'envmon-missing-shell-executable (list envmon-shell-executable))))

(defsubst envmon--shell-executable-label (&optional shell-executable)
  "Interned basename of SHELL-EXECUTABLE."
  (let* ((shell-executable (or shell-executable (envmon-shell-executable))))
    (intern (downcase (file-name-base shell-executable)))))

(defun envmon--shell-executable-default-arguments (&optional label)
  "Convenience method around alits-get with LABEL for default-arguemnts."
  (let ((label (or label (envmon--shell-executable-label))))
    (alist-get label envmon-shell-executable-default-arguments-alist)))

;;;###autoload
(defun envmon-shell-executable-arguments ()
  "Allow user override of default arguemnts map."
  (or envmon-shell-executable-arguments (envmon--shell-executable-default-arguments)))

(defcustom envmon-process-buffer-name "*envmon-host*"
  "Host buffer for `envmon' shell execution."
  :group 'envmon
  :type 'string)

(defcustom envmon-process-stderr-buffer-name "*envmon-stderr*"
  "Stderr buffer for `envmon' shell exectuion."
  :group 'envmon
  :type 'string)

(defvar envmon--env-command '("/usr/bin/env -0")
  "Internal. env program exectuion script.")

(defcustom envmon-async-processing-enabled nil
  "Allow environment management to happen asynchronously."
  :group 'envmon
  :type 'boolean)

(defcustom envmon-shell-execution-timeout 3
  "Timeout for synchronous execution.

NB. Set to zero to disable timeout."
  :group 'envmon
  :type 'number)

(define-error
 'envmon-process-timeout
 "[envmon] Timeout reached awaiting shell execution"
 '(envmon-error))

(defun envmon--spin-lock (process)
  "Block on completion of PROCESS."
  (unless envmon-async-processing-enabled
    (let* ((start-time (float-time)))
      (while (eq 'run (process-status process))
        (unless (zerop envmon-shell-execution-timeout)
          (when (> (- (float-time) start-time) envmon-shell-execution-timeout)
            (signal 'envmon-process-timeout nil)))
        (sleep-for 0.1)))))

(defun envmon--parse-new-environment (buffer)
  "Parse contents of BUFFER as an environment-alist."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let* ((buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
           (null-byte-string (string ?\C-@))
           (environment-lines (string-split buffer-contents null-byte-string)))
      (envmon--filter-environment-alist
       (seq-map #'envmon--parse-environment-cons environment-lines)))))

(defun envmon--apply-new-environment (environment-alist)
  "Apply contents of ENVIRONMENT-ALIST to Emacs."
  (envmon-snapshot-generation environment-alist)
  (envmon--set-environment-alist environment-alist)
  (envmon--update-symbols environment-alist))

(define-error 'envmon-sentinel-error "[envmon] Base sentinel error" '(envmon-error))

(define-error
 'envmon-sentinel-nonsense-process-status
 "[envmon] Nonsense process status in sentinel"
 '(envmon-sentinel-error))

(define-error
 'envmon-sentinel-missing-process
 "[envmon] Nonsense process status in sentinel"
 '(envmon-sentinel-error))

(define-error
 'envmon-sentinel-unknown-status
 "[envmon] Unhanded process status in sentinel"
 '(envmon-sentinel-error))

(defun envmon--host-sentinel (process event)
  "Sentinel for `envmon--make-process' PROCESS to handle EVENT."
  (let* ((status (process-status process)) (signal-data (list status event)))
    (pcase status
      ((or 'run 'stop))
      ((or 'signal 'open 'closed 'connect 'failed 'listen)
       (signal 'envmon-sentinel-nonsense-process-status signal-data))
      ((pred null)
       (signal 'envmon-sentinel-missing-process signal-data))
      ('exit
       (envmon--message "Shell execution complete, ingesting environment")
       (let* ((proc-buffer (process-buffer process))
              (environment-alist (envmon--parse-new-environment proc-buffer)))
         (envmon--apply-new-environment environment-alist)))
      (_
       (signal 'envmon-sentinel-unknown-status signal-data)))))

(defun envmon--make-process (&rest args)
  "Create new shell process.

Most ARGS have defaults."
  (envmon--message
   "Executing envmon in %ssynchronous mode" (if envmon-async-processing-enabled "a" ""))
  (let* ((executable (or (plist-get args :executable) (envmon-shell-executable)))
         (arguments (or (plist-get args :arguments) (envmon-shell-executable-arguments)))
         (buffer (get-buffer-create (or (plist-get args :buffer) envmon-process-buffer-name))))
    (with-current-buffer buffer
      (erase-buffer)
      (envmon--spin-lock
       (make-process
        :name (format "envmon[%s]" (envmon--shell-executable-label executable))
        :buffer buffer
        :command (append (list executable) arguments envmon--env-command)
        :stderr (or (plist-get args :stderr) envmon-process-stderr-buffer-name)
        :sentinel #'envmon--host-sentinel)))))

;;;###autoload
(define-minor-mode envmon-mode
  "Ingest environment variables from POSIX shell."
  :group 'envmon
  :global t
  :lighter envmon-mode-lighter
  (if envmon-mode
      (progn
        (setq envmon--generation-history-alist nil)
        (envmon--make-process))
    (when envmon--generation-history-alist
      (envmon-rollback-generation))))

;;;###autoload
(defun envmon-mode-enable ()
  "Unconditionally enable `envmon-mode'."
  (envmon-mode +1))

;;;###autoload
(defun envmon-mode-disable ()
  "Unconditionally disable `envmon-mode'."
  (envmon-mode -1))

(provide 'envmon)

;;; envmon.el ends here
