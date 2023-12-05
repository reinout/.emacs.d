;;; init.el --- Reinout's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023 Reinout van Rees

;; Author: Reinout van Rees <reinout@vanrees.org>
;; Keywords: internal
;; URL: https://github.com/reinout/.emacs.d/

;;; Commentary:
;; TODO

;;; Code:

;; Use the build-in package system.
(require 'package)
;; Don't start every package right away.
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; List of packages that I use. A really important one is the
;; "better-defaults" one, with really useful small customizations.
(defvar my-packages '(ag
                      better-defaults
                      company
		      exec-path-from-shell
		      magit
                      markdown-mode
		      modus-themes
		      )
  "A list of packages to ensure are installed at launch.")

;; Commands suggested by the Emacs starter kit to keep its packages up
;; to date.
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; exec-path-from-shell ensures the $PATH is set just like in your
;; terminal. This helps finding pipx-installed tools.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; TODO: look at possible font settings.

;; Theme settings
(require 'modus-themes)
(load-theme 'modus-vivendi-tinted :no-confirm)
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      )

;; No escape codes but colors when calling, for instance, 'ruff' in
;; ctrl-c ctrl-v.
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Nicer, smoother scrolling.
(pixel-scroll-mode)
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 35.0)

;; Maximize screen.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Stop ringing the bell
(setq ring-bell-function 'ignore)

;; Change yes/no questions to y/n
(setq use-short-answers t)

;; Start the server so I can use emacsclient
(server-start)

;; Don't let minified javascript (with its super-long lines) bring
;; emacs to a grinding halt.
(global-so-long-mode t)

;; Save the "alt-x" history.
(savehist-mode 1)

;; Don't accidentally exit emacs. Note: this means you have to
;; manually exit emacs when doing a mac OS update.
(setq confirm-kill-emacs 'yes-or-no-p)

;; Indent with spaces instead of tabs by default. Normally the major
;; mode sets it, but I've been bitten by tabs in some rare cases.
(setq-default indent-tabs-mode nil)

;; Auto revert mode: automatically reload the buffer when the
;; underlying file changes (for instance because you ran 'black' over
;; some files. Also keep the directory listings up-to-date.
(global-auto-revert-mode 1)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; When regularly editing, show the current line lightly highlighted.
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; Always turn on flymake when programming. TODO: also for markdown?
(add-hook 'prog-mode-hook #'flymake-mode)

;; Zap trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; When running output is printed in some buffer, just follow along
;; (until the first error).
(setq compilation-scroll-output 'first-error)

;; ;; Completion. One column, max 15 lines.
;; (setq completions-format 'one-column)
;; (setq completions-max-height 15)
;; ;; Config for the new emacs 29+ completion
;; (setq completion-auto-help 'lazy
;;         completion-auto-select 'second-tab
;;         completion-show-help nil
;;         completions-sort nil
;;         completions-header-format nil)

;; Easy ido-style completion.
(fido-vertical-mode)

;; c-x k now directly kills the *current* buffer instead of asking you for the
;; name of a buffer to kill
;; See http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(defun bjm/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)

;; Place the something~ backup files in the temp dir.
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))

;; Python configuration.
(use-package python
  :config
  (require 'eglot)
  (setq python-check-command "ruff")
  (add-hook 'python-mode-hook #'flymake-mode)
  (add-hook 'python-ts-mode-hook #'flymake-mode)
  ;; (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) "ruff-lsp"))
  )

;; Handy f5-shortcuts
(define-prefix-command 'reinout-bindings-keymap)
(define-key reinout-bindings-keymap (vector ?a) 'ag-project)
;(define-key reinout-bindings-keymap (vector ?d) 'deft)
(define-key reinout-bindings-keymap (vector ?f) 'auto-fill-mode)
(define-key reinout-bindings-keymap (vector ?g) 'magit-status)
;(define-key reinout-bindings-keymap (vector ?j) 'jslint-thisfile)
;(define-key reinout-bindings-keymap (vector ?p) 'projectile-command-map)
(define-key reinout-bindings-keymap (vector ?s) 'sort-lines)
;(define-key reinout-bindings-keymap (vector ?t) 'treemacs)
(define-key reinout-bindings-keymap (vector ?u) 'unfill-paragraph)
(global-set-key [(f5)] 'reinout-bindings-keymap)

;; http://koansys.com/tech/emacs-hangs-on-flymake-under-os-x
(setq flymake-gui-warnings-enabled nil)

;; Have customize write its stuff to a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(provide 'init.el)
;;; init.el ends here
