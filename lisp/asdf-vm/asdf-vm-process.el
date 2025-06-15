;;; asdf-vm-process.el --- ASDF VM porceline for Emacs -*- lexical-binding: t -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.3.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config/main/blob/lisp/asdf-vm-process
;; Keywords: languages asdf

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

(require 'asdf-vm-error)
(require 'asdf-vm-util)

(defgroup asdf-vm-process nil
  "Asdf async inferior process configuration group."
  :prefix "asdf-vm-process-"
  :group 'asdf-vm
  :group 'processes)

(defcustom asdf-vm-process-executable (executable-find "asdf")
  "Path to asdf command line tool."
  :type 'string
  :group 'asdf-vm-process)

(defcustom asdf-vm-process-executable-arguments nil
  "Asdf command line tool execution arguments.

These values will be passed to every invocation of asdf before any
command or command arguments."
  :type 'string
  :group 'asdf-vm-process)

(defcustom asdf-vm-process-buffer-name "*asdf-vm*"
  "Host buffer name for async asdf process."
  :type 'string
  :group 'asdf-vm-process)

(defcustom asdf-vm-process-stderr-buffer-name "*asdf-vm-stderr*"
  "Host buffer name for asdf process stderr."
  :type 'string
  :group 'asdf-vm-process)

(defcustom asdf-vm-process-output-buffer-name "*asdf-vm-stdout*"
  "Host buffer name for synchronous asdf process output."
  :type 'string
  :group 'asdf-vm-process)

(defcustom asdf-vm-process-execution-timeout 5
  "Timeout for synchronous execution.

NB. When this value is 0, timeouts are disabled."
  :type 'number
  :group 'asdf-vm-process)

(defvar asdf-vm-process--call-queue nil
  "List of blocked async asdf calls.

These should be ultimately processed by the mutually recursive
`asdf-vm-process--sentinel' and `asdf-vm-call' functions.")

(defun asdf-vm-process--sentinel (process event)
  "Sentinel function used by the `asdf-vm-process--make-process' function.

Reads the state of PROCESS and when execution has completed will check
`asdf-vm-process--call-queue' for async work which is waiting to be
called.

The EVENT value is only ever used for error reporting."
  (let* ((status (process-status process)) (signal-data (list status event)))
    (pcase status
      ((or 'run 'stop))
      ((or 'signal 'open 'closed 'connect 'failed 'listen)
       (signal 'asdf-vm-sentinel-nonsense-process-status signal-data))
      ((pred null)
       (signal 'asdf-vm-sentinel-missing-process signal-data))
      ('exit
       (if asdf-vm-process--call-queue
           (progn
             (asdf-vm-message "Execution complete, processing call queue")
             (let ((args (pop asdf-vm-process--call-queue)))
               (apply #'asdf-vm-call args)))
         (asdf-vm-message "Execution complete")))
      (_
       (signal 'asdf-vm-sentinel-unknown-status signal-data)))))

(defun asdf-vm-process--spin-lock-buffer (buffer)
  "Block on completion of the process owned by BUFFER."
  (when-let* ((buffer (get-buffer buffer))
              (process (get-buffer-process buffer))
              (start-time (float-time)))
    (while (eq 'run (process-status process))
      (unless (zerop asdf-vm-process-execution-timeout)
        (when (> (- (float-time) start-time) asdf-vm-process-execution-timeout)
          (signal 'asdf-vm-process-timeout nil)))
      (sleep-for 0.1))))

(defsubst asdf-vm-process--format-name (name name-prefix command)
  "Internal function to clean up the implementation of `asdf-vm--make-process'.

Attempts to make a pretty human value of of NAME, NAME-PREFIX, and COMMAND."
  (cond (name name)
        (command (format "%s[%s]" name-prefix command))
        (t name-prefix)))

(defun asdf-vm-process--make-process (&rest args)
  "Wrapper function for `make-process'.

ARGS is defined as in `asdf-vm-call' with the execption that
`asdf-vm-process--make-process' only understands :executable,
:executable-arguments, :command, :command-arguments, :directory,
:buffer-name, :name-prefix, and :name.

Given how much munging occurs in `asdf-vm-call' this function should
never be called directly."
  (let* ((executable (plist-get args :executable))
         (executable-arguments (plist-get args :executable-arguments))
         (command (plist-get args :command))
         (command-arguments (plist-get args :command-arguments))
         (directory (plist-get args :directory))
         (buffer (get-buffer-create (plist-get args :buffer-name)))
         (name-prefix (or (plist-get args :name-prefix) (file-name-base executable)))
         (name (asdf-vm-process--format-name (plist-get args :name) name-prefix command)))
    (with-current-buffer buffer
      (let* ((default-directory directory))
        (make-process
         :name name
         :buffer buffer
         :sentinel #'asdf-vm-process--sentinel
         :command `(,executable
                    ,@executable-arguments
                    ,@(seq-map #'symbol-name (if (atom command) (list command) command))
                    ,@command-arguments)
         :stderr asdf-vm-process-stderr-buffer-name)))))

(defun asdf-vm-process--buffer-process-running-p (buffer-name)
  "Check if there is a running process conneced to BUFFER-NAME."
  (and-let* ((buffer (get-buffer buffer-name))
             (process (get-buffer-process buffer)))
    (eq 'run (process-status process))))

(defconst asdf-vm-process--make-process-keys
  '(:name :name-prefix :executable :executable-arguments :command :command-arguments :directory :buffer-name)
  "List of plist keywords understood by `asdf-vm-process--make-process'.")

(defun asdf-vm--make-process-defaults ()
  "Default values for an `asdf-vm--make-process' args list."
  (let ((directory (or (and-let* ((buffer-file (buffer-file-name (current-buffer))))
                    (file-name-parent-directory buffer-file))
                  default-directory)))
    (list
     :executable asdf-vm-process-executable
     :executable-arguments asdf-vm-process-executable-arguments
     :directory directory)))

;;;###autoload
(defun asdf-vm-call (&rest args)
  "Start or enqueue a asdf subprocess. Return the process or result of execution.

This is similar to `make-process' though the arguments carry special
meaning and defaults.

The keyword values for the ARGS plist are as follows:

:name NAME -- NAME is name for the process. When not provided a name
will be construced from :name-prefix and :command.

:name-prefix NAME-PREFIX -- NAME-PREFIX is used in the construction of
NAME. It defaults to the basename of :executable.

:executable EXECUTABLE -- EXECUTABLE is the path to the executable
program to be run in the subprocess. The values defaults to
`asdf-vm-process-executable'.

:executable-arguments EXECUTABLE-ARGUMENTS -- EXECUTABLE-ARGUMENTS is a
list of strings to be passed directly after :executable and before
:command in subprocess execution. This value defaults to
`asdf-vm-process-executable-arguments'.

:command COMMAND -- COMMAND a symbol or list of sumbols representing the
subcommand of EXECUTABLE and is the first argument passed after
:executable-arguments.

:command-arguments COMMAND-ARGUMENTS -- COMMAND-ARGUMENTS is a list of
strings which is the argument values passed after :command during
subprocess execution.

:directory DIRECTORY -- DIRECTORY is the directory path in which the
subprocess is spanwed. This value will default either to the parent of
the current buffer or, if there is not one, to `default-directory'.

:buffer-name BUFFER-NAME -- BUFFER-NAME is the name of the buffer which
hosts the spawned subprocess. This value defaults based on the kind of
execution occuring. For asynchronous called the value is
`asdf-vm-process-buffer-name' and for synchronous calls it is
`asdf-vm-process-output-buffer-name'.

:output OUTPUT -- OUTPUT is a boolean flag indicating that the
`asdf-vm-call' process should both block on execution and return the
string result from subprocess exection.

:blocking BLOCKING -- BLOCKING is a boolean flag indicat taht the
`asdf-vm-call' should be made synchronously.

:success-codes SUCCESS-CODES -- SUCCESS-CODES is a list of integer
values between 1 and 255 which indicate subprocess execution success.
This list always has 0 pushed to the front.

When neighter :blocking, nor :output is set to true, execution will be
enqueued for asychronous execution. This execution starts immediately if
the queue is empty, otherwise it is kicked off immediately after the
completion of the previous asdf subprocess by `asdf-vm-process--sentinel'"
  (let* ((output (plist-get args :output))
         (blocking (or output (plist-get args :blocking)))
         (success-codes (append '(0) (plist-get args :success-codes)))
         (buffer-name (if blocking asdf-vm-process-output-buffer-name asdf-vm-process-buffer-name))
         (call-args (plist-put (asdf-vm--make-process-defaults) :buffer-name buffer-name)))
    (dolist (key asdf-vm-process--make-process-keys)
      (when-let* ((value (plist-get args key)))
        (setq call-args (plist-put call-args key value))))
    (when (and blocking (asdf-vm-process--buffer-process-running-p buffer-name))
      (asdf-vm-process--spin-lock-buffer buffer-name))
    (when output
      (with-current-buffer (get-buffer-create buffer-name) (erase-buffer)))
    (if (and (not blocking) (asdf-vm-process--buffer-process-running-p buffer-name))
        (setq asdf-vm-process--call-queue (append asdf-vm-process--call-queue (list call-args)))
      (let* ((process (apply #'asdf-vm-process--make-process call-args)))
        (when blocking
          (asdf-vm-process--spin-lock-buffer buffer-name))
        (when blocking
          (let* ((process-exit-status (process-exit-status process)))
            (unless (memq process-exit-status success-codes)
              (signal 'asdf-vm-exec-error (list process-exit-status)))))
        (if output
            (with-current-buffer buffer-name
              (buffer-substring-no-properties (point-min) (point-max)))
          process)))))

(provide 'asdf-vm-process)

;;; asdf-vm-process.el ends here
