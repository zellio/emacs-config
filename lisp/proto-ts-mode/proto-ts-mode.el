;;; proto-ts-mode.el --- Treesitter mode for protobuf files -*- lexical-binding: t -*-

;; Copyright (C) 2025 Zachary Elliott

;; Author: Zachary Elliott <contact@zell.io>
;; Maintainer: Zachary Elliott <contact@zell.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0"))
;; Homepage: https://github.com/zellio/emacs-config
;; Keywords: protobuf languages tree-sitter

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

;; This package provides a major mode called `proto-ts-mode' for Protobuf
;; versions 2 and 3.

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-major-mode-setup "treesit")
(declare-function treesit-node-match-p "treesit.c")
(declare-function treesit-node-text "treesit")
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-ready-p "treesit")
(declare-function treesit-search-subtree "treesit.c")

(defgroup proto-ts-mode nil
  "Configuration group for proto-ts-mode."
  :prefix "proto-ts-mode-"
  :group 'tools)

;;; Custom variables

(defcustom proto-ts-mode-indent-offset 2
  "Default indentation offset for Protobuf."
  :type 'integer
  :safe 'integerp
  :group 'proto-ts-mode)

;;; Constants

(defconst proto-ts-mode--keywords
  ["extend" "extensions" "oneof" "option" "reserved" "syntax" "to"]
  "Protobuf language generic keywords for `protobbuf-ts-mode'.")

(defconst proto-ts-mode--keyword-types
  ["enum" "service" "message"]
  "Protobuf language type keywords for `protobbuf-ts-mode'.")

(defconst proto-ts-mode--keyword-functions
  ["rpc"]
  "Protobuf language function definition keywords for `protobbuf-ts-mode'.")

(defconst proto-ts-mode--keyword-modifiers
  ["optional" "repeated" "required"]
  "Protobuf language modifier keywords for `protobbuf-ts-mode'.")

(defconst proto-ts-mode--keyword-imports
  ["package" "import"]
  "Protobuf language import keywords for `protobbuf-ts-mode'.")

(defconst proto-ts-mode--keyword-returns
  ["returns"]
  "Protobuf language return keywords for `protobbuf-ts-mode'.")

(defconst proto-ts-mode--punctuation-brackets
  ["(" ")" "[" "]" "{" "}" "<" ">"]
  "Protobuf language brackets for `protobbuf-ts-mode'.")

(defconst proto-ts-mode--punctuation-delimiters
  [";" ","]
  "Protobuf language delimiters for `protobbuf-ts-mode'.")

(defconst proto-ts-mode--punctuation-operators
  ["="]
  "Protobuf language operators for `protobbuf-ts-mode'.")

(defconst proto-ts-mode--primitive-types
  ["double" "float" "int32" "int64" "uint32" "uint64" "sint32" "sint64"
   "fixed32" "fixed64" "sfixed32" "sfixed64" "bool" "string" "bytes"]
  "Protobuf language built in types for `protobbuf-ts-mode'.")

;;; Indentation

(defconst proto-ts-mode--simple-indent-rules
  `((proto
     ((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "service") parent-bol proto-ts-mode-indent-offset)
     ((parent-is "rpc") parent-bol proto-ts-mode-indent-offset)
     ((parent-is "message") parent-bol proto-ts-mode-indent-offset)
     ((parent-is "enum") parent-bol proto-ts-mode-indent-offset)
     ((parent-is "oneof") parent-bol proto-ts-mode-indent-offset)))
  "`treesit-simple-indent-rules' for `proto-ts-mode'.")

;;; Font-lock

(defvar proto-ts-mode--font-lock-feature-list
  '((comment)
    (keyword function-name type string)
    (number)
    (punctuation))
  "`treesit-font-lock-feature-list' for `proto-ts-mode'.")

(defconst proto-ts-mode--font-lock-rules
  `(:language proto
    :feature keyword
    ((syntax) @font-lock-keyword-face
     ,proto-ts-mode--keywords @font-lock-keyword-face
     ,proto-ts-mode--keyword-types @font-lock-keyword-face
     ,proto-ts-mode--keyword-functions @font-lock-keyword-face
     ,proto-ts-mode--keyword-modifiers @font-lock-keyword-face
     ,proto-ts-mode--keyword-imports @font-lock-keyword-face
     ,proto-ts-mode--keyword-returns @font-lock-keyword-face)

    :language proto
    :override t
    :feature type
    ((service_name (identifier) @font-lock-type-face)
     (message_name (identifier) @font-lock-type-face)
     (enum_name (identifier) @font-lock-type-face)
     (rpc_name (identifier) @font-lock-function-name-face)
     (message_or_enum_type (identifier) @font-lock-type-face)
     "map" @font-lock-type-face
     (type ,proto-ts-mode--primitive-types) @font-lock-builtin-face
     (key_type) @font-lock-builtin-face)

    :language proto
    :feature punctuation
    (,proto-ts-mode--punctuation-brackets @font-lock-bracket-face
     ,proto-ts-mode--punctuation-delimiters @font-lock-delimiter-face
     ,proto-ts-mode--punctuation-operators @font-lock-operator-face)

    :language proto
    :feature string
    ((string) @font-lock-string-face)

    :language proto
    :feature number
    ((int_lit) @font-lock-number-face)

    :language proto
    :override t
    :feature comment
    ((comment) @font-lock-comment-face))
  "`treesit-font-lock-rules' for `proto-ts-mode'.")

;;; Imenu

(defun proto-ts-mode--defun-name-function (node)
  "Return the defun name of NODE."
  (treesit-node-text
   (treesit-search-subtree node (rx string-start "identifier" string-end) nil t) t))

(defconst proto-ts-mode--simple-imenu-settings
  `(("Service" ,(rx string-start "service_name" string-end) nil nil)
    ("RPC" ,(rx string-start "rpc_name" string-end) nil nil)
    ("Message" ,(rx string-start "message_name" string-end) nil nil)
    ("Enum" ,(rx string-start "enum_name" string-end) nil nil))
  "`treesit-simple-imenu-settings' for `proto-ts-mode'.")

;;; Outline minor mode

(defun proto-ts-mode--outline-predicate (node)
  "Match outlines when NODE is a block opener."
  (treesit-node-match-p node 'sentence))

;;; Things

(defconst proto-ts-mode--things-settings
  `((proto
     (comment ,(rx string-start "comment" string-end))
     (string ,(rx string-start "string" string-end))
     (text ,(rx string-start (or "comment" "string") string-end))
     (sexp (not ,(regexp-opt (append
                              proto-ts-mode--punctuation-brackets
                              proto-ts-mode--punctuation-operators
                              '(",")))))
     (sentence ,(rx string-start (or "service" "message" "enum") string-end))
     (defun sentence)))
  "`treesit-thing-settings' for `proto-ts-mode'.")

;;; Mode

(defvar-keymap proto-ts-mode-map
  :doc "Keymap for potobuf with tree-sitter."
  :parent prog-mode-map)

;;;###autoload
(define-derived-mode proto-ts-mode prog-mode "Proto[ts]"
  "Major mode for editing Protobuf with tree-sitter."
  :group 'proto-ts-mode
  :syntax-table nil
  (when (treesit-ready-p 'proto)
    (treesit-parser-create 'proto)

    (setq-local
     comment-start "// "
     comment-end ""
     comment-start-skip (rx "//" (* (syntax whitespace)))

     treesit-font-lock-settings (apply #'treesit-font-lock-rules proto-ts-mode--font-lock-rules)
     treesit-font-lock-feature-list proto-ts-mode--font-lock-feature-list
     treesit-simple-indent-rules proto-ts-mode--simple-indent-rules
     treesit-simple-imenu-settings proto-ts-mode--simple-imenu-settings
     treesit-defun-type-regexp (rx string-start (or "service" "rpc" "message" "enum") string-end)
     treesit-defun-name-function #'proto-ts-mode--defun-name-function
     treesit-outline-predicate #'proto-ts-mode--outline-predicate
     treesit-thing-settings proto-ts-mode--things-settings)

    (treesit-major-mode-setup)))

(when (treesit-ready-p 'proto)
  (add-to-list 'auto-mode-alist (cons (rx ".proto" line-end) 'proto-ts-mode)))

(provide 'proto-ts-mode)

;;; proto-ts-mode.el ends here
