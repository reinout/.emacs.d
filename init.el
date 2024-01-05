;;; init.el --- Reinout's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023 Reinout van Rees

;; Author: Reinout van Rees <reinout@vanrees.org>
;; Keywords: internal
;; URL: https://github.com/reinout/.emacs.d/

;;; Commentary:
;; See the readme.

;;; Code:

;; Don't start every (previously installed) package right away.
(setq package-enable-at-startup nil)
;; Use the standard melpa archive, too.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; Download package info when it didn't happen yet.
(when (not package-archive-contents)
  (package-refresh-contents))


;; better-defaults is the set of nicer settings that previously was
;; part of the "emacs starter kit".
(use-package better-defaults
  :ensure t
  :init
  ; first start fido, otherwise ido already gets started.
  (fido-vertical-mode)
  )

;; exec-path-from-shell ensures the $PATH is set just like in your
;; terminal. This helps finding pipx-installed tools when starting
;; emacs from your OS
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )

;; Start the server so I can use emacsclient.
(use-package server
  :if window-system
  :init
  (server-start)
  ;; Maximize screen when starting up.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  )

;; Theme settings. I kinda like this dark theme (there's a white
;; version, too, btw).
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)
  (load-theme 'modus-vivendi-tinted :no-confirm)
  )

;; No escape codes but colors when calling, for instance, 'ruff' in
;; ctrl-c ctrl-v.
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter)
  )

;; Nicer, smoother scrolling.
(use-package pixel-scroll
  :config
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0)
  )

;; Handy for visually wrapping lines on the fill column instead of at
;; the end of the screen in case a colleague has written a readme with
;; really long lines. I've configured visual-line-mode as "ctrl-c v".
(use-package visual-fill-column
  :ensure t
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  :bind ("C-c v" . visual-line-mode)
  )

;; Useful for working with abovementioned colleague-written README
;; files with long lines, "ctrl-c u" restores the long line if I've
;; accidentally filled it.
;; Included in this package config is my "ctrl-c f" toggle for
;; auto-fill-mode.
(use-package unfill
  :ensure t
  :bind (("C-c u" . unfill-paragraph)
         ("C-c f" . auto-fill-mode))
  )

;; Let flymake use alt-n/alt-p for moving to the next/previous error.
(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-previous-error))
  )

;; Yasnippet library
(use-package yasnippet-snippets
  :ensure t
  )

;; Yaml editing
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'"
  )

;; Snippets (TODO: I need to add some of my own)
(use-package yasnippet
  :ensure t
  :init
  ;; add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode t)
  )

(defun projectile-reinout-test ()
  "Run github.com/reinout/tools/shell/projectile-test in the project root"
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (compile "projectile-test")
    )
  )

(defun projectile-reinout-check ()
  "Run github.com/reinout/tools/shell/projectile-check in the project root"
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (compile "projectile-check")
    )
  )

(defun projectile-reinout-beautiful ()
  "Run github.com/reinout/tools/shell/projectile-beautiful in the project root"
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (compile "projectile-beautiful")
    )
  )


;; Projectile (project support) installation.
(use-package projectile
  :ensure t
  :init
  (projectile-mode 1)
  :config
  (setq projectile-test-cmd "projectile-test")
  :bind-keymap ("C-c p" . projectile-command-map)
  ;; Adjustments
  ;; :bind (("C-c p p" . projectile-switch-open-project)
  ;;        )
  ;; Handy short keystrokes
  :bind (("C-c t" . projectile-reinout-test)
         ("C-c c" . projectile-reinout-check)
         ("C-c b" . projectile-reinout-beautiful)
         )
  :custom
  (projectile-switch-project-action 'projectile-dired "Open root dir")
  (projectile-mode-line-prefix "P" "Shorter prefix")
  )

;; Show available key bindings.
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

;; ag is a really nice grep variant. "ctrl-c a" greps inside the
;; current project.
(use-package ag
  :ensure t
  :bind ("C-c a" . ag-project)
  )

;; Really nice and powerful git integration.
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  )

;; Python configuration.
(use-package python
  :config
  (setq python-check-command "ruff")
  (add-hook 'python-base-mode-hook 'eglot-ensure)
  )

;; COMplete ANYthing. Integrates with LSP, but can also be used
;; outside it.
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  )

;; LSP through eglot
(use-package eglot
  :bind (("C-c l c" . eglot-reconnect)
         ("C-c l d" . flymake-show-buffer-diagnostics)
         ("C-c l f f" . eglot-format)
         ("C-c l f b" . eglot-format-buffer)
         ("C-c l l" . eglot)
         ("C-c l r n" . eglot-rename)
         ("C-c l s" . eglot-shutdown))
  )

;; Dockerfile
(use-package dockerfile-mode
  :ensure t
  )

;; Generic emacs configuration.
(use-package emacs
  :init

  ;; Stop ringing the bell
  (setq ring-bell-function 'ignore)

  ;; Change yes/no questions to y/n
  (setq use-short-answers t)

  ;; Don't let minified javascript (with its super-long lines) bring
  ;; emacs to a grinding halt.
  (global-so-long-mode t)

  ;; Save the "alt-x" history.
  (savehist-mode 1)

  ;; Don't accidentally exit emacs. Note: this means you have to
  ;; manually exit emacs when doing a mac OS update.
  (setq confirm-kill-emacs 'yes-or-no-p)

  ;; Indent with spaces instead of tabs by default. Normally the major
  ;; mode sets it, but I've been bitten by tabs in some rare
  ;; cases. Makefile-mode correctly uses tabs, which is the only place
  ;; where I need them.
  (setq-default indent-tabs-mode nil)

  ;; Auto revert mode: automatically reload the buffer when the
  ;; underlying file changes (for instance because you ran 'black' over
  ;; some files. Also keep the directory listings up-to-date.
  (global-auto-revert-mode 1)
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  ;; When regularly editing, show the current line lightly highlighted.
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode)

  ;; Zap trailing whitespace.
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; When running output is printed in some buffer, just follow along
  ;; (until the first error).
  (setq compilation-scroll-output 'first-error)

  ;; Place the something~ backup files in the temp dir.
  (setq backup-directory-alist
            `((".*" . ,temporary-file-directory)))


  :bind (("C-c s" . sort-lines)
         )
  )


;; c-x k now directly kills the *current* buffer instead of asking you for the
;; name of a buffer to kill
;; See http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(defun bjm/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)


;; Have customize write its stuff to a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(provide 'init.el)
;;; init.el ends here
