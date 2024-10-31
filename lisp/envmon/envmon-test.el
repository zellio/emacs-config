;;; envmon-test.el --- Testing of envmon.el -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: http://github.com/zellio/emacs-config/blob/main/lisp/envmon

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

(require 'envmon)
(require 'ert)

(ert-deftest envmon--get-environment-cons-test ()
  (let* ((process-environment (copy-sequence process-environment)))
    (should (equal '("FOO" . nil) (envmon--get-environment-cons "FOO")))
    (setenv "TEST_VAR_001" "TEST_VAL_001")
    (should (equal '("TEST_VAR_001" . "TEST_VAL_001")
                   (envmon--get-environment-cons "TEST_VAR_001")))))

(ert-deftest envmon--get-environment-alist-test ()
  (let* ((process-environment (copy-sequence process-environment)))
    (setenv "TEST_ENV_VAR_001" "TEST_ENV_VAL_001")
    (should (equal '(("FOO" . nil) ("TEST_ENV_VAR_001" . "TEST_ENV_VAL_001"))
                   (envmon--get-environment-alist '("FOO" "TEST_ENV_VAR_001"))))))

(ert-deftest envmon--set-environment-alist-test ()
  (let* ((process-environment (copy-sequence process-environment)))
    (envmon--set-environment-alist '(("FOO" . "TEST_ENV_VAR_VAL_001")))
    (should (equal "TEST_ENV_VAR_VAL_001" (getenv "FOO")))))

(ert-deftest envmon-with-environment-alist-test ()
  (should (equal "BAR" (envmon-with-environment-alist (("FOO" . "BAR")) (getenv "FOO"))))
  (should (equal nil (getenv "FOO"))))

(ert-deftest envmon--format-environment-cons-test ()
  (let* ((process-environment (copy-sequence process-environment)))
    (should (equal "FOO" (envmon--format-environment-cons '("FOO" . nil))))
    (should (equal "FOO=bar" (envmon--format-environment-cons '("FOO" . "bar"))))))

(ert-deftest envmon--parse-environment-cons-test ()
  (let* ((process-environment (copy-sequence process-environment)))
    (should (equal '("FOO" . nil) (envmon--parse-environment-cons "FOO")))
    (should (equal '("FOO" . "") (envmon--parse-environment-cons "FOO=")))
    (should (equal '("FOO" . "Hello world = how are you?")
                   (envmon--parse-environment-cons "FOO=Hello world = how are you?")))))

(ert-deftest evnmon--format-environment-alist-test ()
  (let* ((process-environment (copy-sequence process-environment)))
    (should (equal "FOO" (envmon--format-environment-alist '(("FOO" . nil)))))
    (should (equal "FOO=bar" (envmon--format-environment-alist '(("FOO" . "bar")))))
    (should (equal "FOO=bar\nBAR=baz"
                   (envmon--format-environment-alist '(("FOO" . "bar") ("BAR" . "baz")))))
    (should (equal "FOO=bar|BAR=baz"
                   (envmon--format-environment-alist '(("FOO" . "bar") ("BAR" . "baz")) "|")))))

(ert-deftest envmon--allow-filter-environment-alist-test ()
  (let ((process-environment (copy-sequence process-environment))
        (environment-alist '(("FOO" . "BAR") ("BAR" . "BAZ") ("BAZ" . "BANG"))))
    (should (equal environment-alist (envmon--allow-filter-environment-alist environment-alist nil)))
    (should (equal environment-alist (envmon--allow-filter-environment-alist environment-alist 'all)))
    (should (equal '(("FOO" . "BAR")) (envmon--allow-filter-environment-alist environment-alist '("FOO"))))))

(ert-deftest envmon--deny-filter-environment-alist-test ()
  (let ((process-environment (copy-sequence process-environment))
        (environment-alist '(("FOO" . "BAR") ("BAR" . "BAZ") ("BAZ" . "BANG"))))
    (should (equal environment-alist
                   (envmon--deny-filter-environment-alist environment-alist nil)))
    (should (equal '(("FOO" . "BAR"))
                   (envmon--deny-filter-environment-alist environment-alist '("BAR" "BAZ"))))))

(ert-deftest envmon--filter-environment-alist-test ()
  (let ((process-environment (copy-sequence process-environment))
        (environment-alist '(("FOO" . "BAR") ("BAR" . "BAZ") ("BAZ" . "BANG"))))
    (should (equal environment-alist
                   (envmon--filter-environment-alist environment-alist nil nil)))
    (should (equal environment-alist
                   (envmon--filter-environment-alist environment-alist 'all nil)))
    (should (equal '(("BAR" . "BAZ"))
                   (envmon--filter-environment-alist environment-alist '("FOO" "BAR") '("FOO"))))))

(ert-deftest envmon--symbol-update-default-test ()
  (defvar test-symbol nil)
  (envmon-with-environment-alist (("TEST_ENV" . "Test value"))
                                 (envmon--symbol-update-default '(test-symbol . ("BAD_ENV" "TEST_ENV")))
                                 (should (equal "Test value" test-symbol))))

(ert-deftest envmon--symbol-update-test ()
  (envmon-with-environment-alist
   (("TEST_ENV" . "Test Value"))
   (let ((envmon--symbol-environment-alist '((test-symbol . ("TEST_ENV")))))
     (defvar test-symbol nil)
     (envmon--symbol-update 'test-symbol)
     (should (equal "Test Value" test-symbol))
     (defun envmon--symbol-update-test-symbol (_symbol-cons)
       (setq test-symbol "Test func ran"))
     (envmon--symbol-update 'test-symbol)
     (should (equal "Test func ran" test-symbol)))))

(ert-deftest envmon--environment-snapshot-test ()
  (envmon-with-environment-alist
   (("FOO" . "bar"))
   (should (equal '(("FOO" . "bar"))
                  (envmon--environment-snapshot '(("FOO" . "BAR")))))))

(ert-deftest envmon--symbols-snapshot-test ()
  (should (equal `((load-path . ,load-path)
				   (user-full-name . ,user-full-name))
                 (envmon--symbols-snapshot '(("NAME" . "BAR") ("EMACSLOADPATH" . "BAZ"))))))

(ert-deftest envmon--generation-snapshot-test ()
  (should (equal 10 (car (envmon--generation-snapshot 10 nil))))
  (should (assoc 'environment (cdr (envmon--generation-snapshot nil nil))))
  (should (assoc 'symbols (cdr (envmon--generation-snapshot nil nil)))))

(ert-deftest envmon-snapshot-generation-test ()
  (setq envmon--generation-history-alist nil)
  (envmon-snapshot-generation '(("NAME" . "BAR")) 0)
  (let ((generation (car envmon--generation-history-alist)))
	(should (zerop (car generation)))
	(should (length= (cdr generation) 2))
	(let-alist (cdr generation)
	  (should (equal `(("NAME" . ,(getenv "NAME"))) .environment))
	  (should (equal  `((user-full-name . ,user-full-name)) .symbols)))))

(ert-deftest envmon-rollback-generation-test ()
  (setq envmon--generation-history-alist nil)
  (let* ((new-name-value "Alice Programmer")
		 (environment-alist `(("NAME" . ,new-name-value))))
	(envmon-snapshot-generation environment-alist)
	(envmon--set-environment-alist environment-alist)
	(envmon--update-symbols environment-alist)
	(should (equal new-name-value user-full-name))
	(should (equal new-name-value (getenv "NAME")))
	(envmon-rollback-generation)
	(should (not (equal new-name-value user-full-name)))
	(should (not (equal new-name-value (getenv "NAME"))))
	(should (length= envmon--generation-history-alist 0))))

(ert-deftest envmon-shell-executable-test ()
  (let* ((tempfile (make-temp-file "envmon-test-"))
		 (exec-path (cons (file-name-parent-directory tempfile) exec-path)))
	(set-file-modes tempfile ?\777)
	(unwind-protect
		(progn
		  (let ((envmon-shell-executable tempfile))
			(should (equal tempfile (envmon-shell-executable))))
		  (let ((envmon-shell-executable (file-name-base tempfile)))
			(should (equal tempfile (envmon-shell-executable)))))
	  (delete-file tempfile)))
  (let ((envmon-shell-executable 'shell-file-name))
	(should (equal shell-file-name (envmon-shell-executable))))
  (let ((envmon-shell-executable 'guess))
	(should (equal shell-file-name (envmon-shell-executable))))
  (let ((envmon-shell-executable nil))
	(should (equal shell-file-name (envmon-shell-executable)))))

(ert-deftest envmon--shell-executable-label-test ()
  (should (equal 'zsh (envmon--shell-executable-label "/bin/zsh")))
  (should (equal 'zsh (envmon--shell-executable-label "/bIn/ZsH"))))

(ert-deftest envmon--shell-executable-default-arguments-test  ()
  (should (null (envmon--shell-executable-default-arguments 'foo)))
  (let* ((default-args '((foo . ("default" "args"))))
		 (envmon-shell-executable-default-arguments-alist `((foo . ,default-args))))
	(should (equal default-args (envmon--shell-executable-default-arguments 'foo)))))

(ert-deftest envmon-shell-executable-arguments-test ()
  (let ((envmon-shell-executable-arguments nil))
	(should (equal (envmon-shell-executable-arguments)
				   (envmon--shell-executable-default-arguments))))
  (let ((envmon-shell-executable-arguments '("test-value")))
	(should (equal (envmon-shell-executable-arguments) '("test-value")))))

(ert-deftest envmon--spin-lock-test ()
  "Unimplemented process test."
  :tags '(process)
  (should nil))

(ert-deftest envmon--parse-new-environment-test ()
  (with-temp-buffer
	(insert (string-join '("FOO=BAR" "BAR=BAZ" "Bang=Hello world") (string ?\C-@)))
	(should (equal '(("FOO" . "BAR") ("BAR" . "BAZ") ("Bang" . "Hello world"))
				   (envmon--parse-new-environment (current-buffer))))))

(ert-deftest envmon--apply-new-environment-test ()
  (let* ((new-name "Bob Programmer")
		 (masked-name (getenv "NAME"))
		 (envmon--generation-history-alist nil)
		 (new-environment-alist `(("NAME" . ,new-name))))
	(envmon--apply-new-environment new-environment-alist)
	(should (length= envmon--generation-history-alist 1))
	(should (equal (assoc "NAME" (assoc 'environment (cadr envmon--generation-history-alist))) masked-name))
	(should (equal new-name (getenv "NAME")))
	(should (equal new-name user-full-name))))

(ert-deftest envmon--host-sentinel-test ()
  "Unimplemented process test.

Covered in part by `envmon--parse-new-environment-test' and `envmon--apply-new-environment-test'."
  :tags '(process)
  (should t))

(ert-deftest envmon--make-process-test ()
  "Unimplemented process test."
  :tags '(process)
  (should nil))

(ert-deftest envmon-mode-enable-test ()
  (envmon-mode-enable)
  (should envmon-mode))

(ert-deftest envmon-mode-disable-test ()
  (envmon-mode-disable)
  (should (not envmon-mode)))

(provide 'envmon-test)

;;; envmon-test.el ends here
