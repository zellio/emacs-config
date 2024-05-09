;;; 99_theme.el --- load emacs theme -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Zachary Elliott
;; See COPYING for more information

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))

(use-package mood-line
  :after (flycheck)

  :preface
  (defconst user/mood-line-segment-left-separator "  ")

  (defconst user/mood-line-segment-right-separator "  ")

  (defun user/sqash-path (path)
    ""
    (let* ((path-components (f-split path)))
      (apply
       #'f-join
       (append
        (mapcar (lambda (component)
                  (seq-take component 1))
                (butlast path-components))
        (last path-components)))
      ))

  (defun user/mood-line-segment-input-info ()
    ""
    (propertize
     (format-mode-line
      '(""
        mode-line-mule-info
        mode-line-modified
        mode-line-client
        mode-line-remote))
     'display '(min-width (5.0))
     ))

  (defun user/mood-line-segment-project-buffer ()
    ""
    (if-let* ((buffer-file (buffer-file-name))
              (project (project-current))
              (project-directory (project-root project)))
        (propertize
         (format
          "%s%s%s"
          (propertize
           (projectile-project-name)
           'face 'mood-line-status-info
           'mouse-face 'mode-line-highlight
           'local-map (make-mode-line-mouse-map 'mouse-1 #'projectile-switch-project))
          #(":" 0 1 (face mood-line-unimportant))
          (user/sqash-path
           (file-relative-name (file-truename buffer-file) project-directory)))
         'help-echo buffer-file)
      (buffer-name)))

  (defun user/mood-line-segment-buffer-position ()
    ""
    (format
     "%s %s"
     (format-mode-line "(%l,%c)")
     (format-mode-line "%o" 'mood-line-unimportant)))

  (defun user/mood-line-segment-vc--state-token (state)
    ""
    (pcase state
      ((or 'edited 'added)
       (nerd-icons-mdicon
        "nf-md-plus"
        'face 'mood-line-status-info))
      ('needs-update
       (nerd-icons-mdicon
        "nf-md-arrow_down"
        'face 'mood-line-status-warning))
      ('needs-merge
       (nerd-icons-mdicon
        "nf-md-arrow_left_right"
        'face 'mood-line-status-warning))
      ((or 'removed 'conflict 'missing 'unregistered)
       (nerd-icons-codicon
        "nf-cod-error"
        'face 'mood-line-status-error))
      ((or 'up-to-date 'ignored)
       (nerd-icons-mdicon
        "nf-md-check"
        'face 'mood-line-status-neutral))
      (_ "")))

  (defun user/mood-line-segment-vc--text ()
    ""
    (when-let* ((buffer-file (and vc-mode (buffer-file-name)))
                (state (vc-state buffer-file))
                (state-token (user/mood-line-segment-vc--state-token state))
                (backend (vc-backend buffer-file))
                (truncate-length (+ 2 (length (symbol-name backend))))
                (revision (seq-drop vc-mode truncate-length))
                (message
                 (format
                  "[%s%s%s]"
                  revision
                  (if (> (length state-token) 0) " " "")
                  state-token)))
      (pcase state
        ((or 'up-to-date 'ignored)
         (propertize message 'face 'mood-line-unimportant))
        (_ message))))

  (defun user/mood-line-segment-flycheck--indicator (&optional status)
    ""
    (let* ((default #("0" 0 1 (face mood-line-unimportant)))
           (status (or status flycheck-last-status-change)))
      (pcase status
        (`not-checked "=")
        (`no-checker #("-" 0 1 (face mood-line-unimportant)))
        (`running (nerd-icons-mdicon "nf-md-sync"))
        (`errored #("!" 0 1 (face mood-line-status-error)))
        (`interrupted #("*" 0 1 (face mood-line-error)))
        (`suspicious "?")
        (`finished
         (let-alist (flycheck-count-errors flycheck-current-errors)
           (format
            "%s %s %s"
            (if .error
                (propertize
                 (format "%d" .error)
                 'face 'mood-line-status-error
                 'help-echo "Goto first error"
                 'mouse-face 'mode-line-highlight
                 'local-map (make-mode-line-mouse-map 'mouse-1 #'flycheck-first-error))
              default)
            (if .warning
                (propertize
                 (format "%d" .warning)
                 'face 'mood-line-status-warning
                 'help-echo "Goto first error"
                 'mouse-face 'mode-line-highlight
                 'local-map (make-mode-line-mouse-map 'mouse-1 #'flycheck-first-error))
              default)
            (if .info
                (propertize
                 (format "%d" .info)
                 'face 'mood-line-status-info)
              default)))
         ))
      ))

  (defun user/mood-line-segment-flycheck ()
    ""
    (when (bound-and-true-p flycheck-mode)
      (let* ((message
              (format
               "%s[%s]"
               (propertize
                "FlyC"
                'help-echo (pcase flycheck-last-status-change
                             (`not-checked "Not Checked")
                             (`no-checker "No Checker")
                             (`running "Running...")
                             (`errored "Error!")
                             (`interrupted "Interrupted")
                             (`suspicious "Suspicious?")
                             (`finished "Display errors found by Flycheck"))
                'display '(raise 0.0)
                'mouse-face 'mode-line-highlight
                'local-map (make-mode-line-mouse-map 'mouse-1 #'flycheck-list-errors))
               (user/mood-line-segment-flycheck--indicator))))
        (if (eq flycheck-last-status-change 'no-checker)
            (propertize message 'face 'mood-line-unimportant)
          message)
      )))

  (defun user/mood-line-segment-eglot ()
    "Compose eglot mode line."
    (when-let* ((server (and (fboundp 'eglot-current-server)
                             (eglot-current-server)))
                (server-info (eglot--server-info server))
                (values
                 `(,(when-let* ((server-name (or (plist-get server-info :name) (jsonrpc-name server))))
                      (propertize
                       server-name
                       'face 'eglot-mode-line
                       'mouse-face 'mode-line-highlight
                       'help-echo "mouse-1: LSP server control menu"
                       'keymap (make-mode-line-mouse-map 'down-mouse-1 #'eglot-server-menu)))
                   ,(when-let* ((last-error (jsonrpc-last-error server)))
                      (propertize
                       "Err"
                       'face 'compilation-mode-line-fail
                       'help-echo "mouse-3: Clear status"
                       'mouse-face 'mode-line-highlight
                       'keymap (make-mode-line-mouse-map 'mouse-3 'eglot-clear-status)))
                   ,(when-let ((pending (jsonrpc-continuation-count server)))
                      (when (cl-plusp pending)
                        (propertize
                         (format "%d" pending)
                         'face 'mood-line-status-warning
                         'help-echo "mouse-3: Forget pending continuations"
                         'mouse-face 'mode-line-highlight
                         'keymap (make-mode-line-mouse-map 'mouse-3 'eglot-forget-pending-continuations))
                        ))
                   ,@(cl-loop
                      for progress-reporter hash-values of (eglot--progress-reporters server)
                      when (eq (car progress-reporter) 'eglot--mode-line-reporter)
                      collect (propertize
                               (format "%s%%" (or (nth 4 progress-reporter) "?"))
                               'help-echo
                               (format
                                "(%s) %s %s"
                                (nth 1 progress-reporter)
                                (nth 2 progress-reporter)
                                (nth 3 progress-reporter)))))
                 ))
      (format
       "%s[%s]"
       (propertize
        "Eglot"
        'mouse-face 'mode-line-highlight
        'local-map (make-mode-line-mouse-map 'down-mouse-1 #'eglot-menu))
       (s-join "/" (flatten-tree values)))))

  :hook (after-init . mood-line-mode)

  :config
  (defun mood-line-segment-vc--update ()
    ""
    (setq
     mood-line-segment-vc--text (user/mood-line-segment-vc--text)))

  :custom
  (mood-line-format
   (mood-line-defformat
    :left
    (((or (mood-line-segment-process)
          (user/mood-line-segment-input-info))
      . user/mood-line-segment-left-separator)
     ((user/mood-line-segment-project-buffer) . user/mood-line-segment-left-separator)
     (user/mood-line-segment-buffer-position))
    :right
    (((user/mood-line-segment-flycheck) . user/mood-line-segment-right-separator)
     ((user/mood-line-segment-eglot) . user/mood-line-segment-right-separator)
     (mood-line-segment-major-mode)
     (when-let* ((vc-mode (mood-line-segment-vc)))
       (format "%s%s" user/mood-line-segment-right-separator vc-mode)))))
  (mood-line-glyph-alist mood-line-glyphs-unicode))

;;; 99_theme.el ends here
