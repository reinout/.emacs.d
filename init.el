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
;; terminal. This helps finding 'uv tool'-installed tools when starting
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
  :init
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

;; I want to add editorconfig files to projects, so I have to use it
;; myself, too.
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Let flymake use alt-n/alt-p for moving to the next/previous error.
(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error))
  )

;; Yasnippet library
(use-package yasnippet-snippets
  :ensure t
  )

(use-package markdown-mode
  :ensure t
  ;; Retain flymake's two keys instead of using them for prev/next link.
  :bind (:map markdown-mode-map
         ("M-n" . nil)
         ("M-p" . nil))
  ;; :config ((require 'bind-key)
  ;;          (unbind-key "M-n" markdown-mode-map)
  ;;          (unbind-key "M-p" markdown-mode-map))
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

;; Direnv loads env vars from .envrc, allowing me to customize the following three
;; "projectile" helper commands with env vars like PROJECTILE_MAKE.
(use-package direnv
  :config
  (direnv-mode)
  (add-hook 'prog-mode-hook #'direnv--maybe-update-environment)
  )

(defun projectile-reinout-test ()
  "Run github.com/reinout/tools/shell/projectile-test in the project root"
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (compile "projectile-test")
    )
  )

(defun projectile-reinout-make ()
  "Run github.com/reinout/tools/shell/projectile-compile in the project root"
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (compile "projectile-make")
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
         ("C-c m" . projectile-reinout-make)
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

;; ripgrep uses rg instead of ag. "ctrl-c r" greps inside the current project (at least,
;; I hope it is within the project)
(use-package ripgrep
  :ensure t
  :bind ("C-c r" . projectile-ripgrep)
  )

;; Really nice and powerful git integration.
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  )
(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.netrc"))
  )


;; Python configuration.
(use-package python
  :config
  (setq python-check-command "ruff")
  )

;; LSP through eglot
(use-package eglot
  :bind (("C-c l c" . eglot-reconnect)
         ("C-c l d" . flymake-show-buffer-diagnostics)
         ; ("C-c l f f" . eglot-format)
         ; ("C-c l f b" . eglot-format-buffer)
         ("C-c l l" . eglot)
         ("<mouse-3>" . eglot-code-actions)
         ("C-c c" . eglot-code-actions)
         ("C-c l r n" . eglot-rename)
         ("C-c l s" . eglot-shutdown)
         ("C-c h" . eglot-inlay-hints-mode)
         )
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("ty" "server")))
  (add-to-list 'eglot-server-programs
               '(conf-toml-mode . ("tombi" "lsp" "--no-cache")))
  )


;; Spelling + grammar checker.
;; See https://github.com/emacs-languagetool/eglot-ltex-plus
(use-package eglot-ltex-plus
  :ensure t
  ;; :hook (text-mode . (lambda ()
  ;;                      (require 'eglot-ltex-plus)
  ;;                      (eglot-ensure)))
  :init
  (setq eglot-ltex-plus-server-path "/opt/homebrew/bin/ltex-ls-plus"
        eglot-ltex-plus-communication-channel 'stdio))         ; 'stdio or 'tcp

;; (use-package flymake-languagetool
;;   :ensure t
;;   :hook ((text-mode       . flymake-languagetool-load)
;;          (latex-mode      . flymake-languagetool-load)
;;          (rst-mode        . flymake-languagetool-load)
;;          (markdown-mode   . flymake-languagetool-load))
;;   :init
;;   ;; If using Premium Version provide the following information
;;   (setq flymake-languagetool-server-jar nil)
;;   (setq flymake-languagetool-url "https://api.languagetoolplus.com")
;;   (setq flymake-languagetool-api-username "reinout@vanrees.org")
;;   (setq flymake-languagetool-api-key "pit-mhchr3JWtcqb"))



;; Completion interface.
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )


;; Dockerfile
(use-package dockerfile-mode
  :ensure t
  )

(use-package terraform-mode
  :ensure t
  :config
  (setq terraform-format-on-save t)
  )

(use-package arduino-mode
  :ensure t
  :mode "\\.ino\\'"
  :hook
  (arduino-mode . (lambda ()
                    (setq-local compile-command "make upload")))
  )

(use-package mermaid-mode
  :ensure t
  :mode "\\.mermaid\\'"
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


;; From https://stackoverflow.com/a/11059012/27401
(defun bury-compile-buffer-if-successful (buffer string)
 "Bury a compilation buffer if succeeded without warnings "
 (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 2 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                    buffer)))


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

  ;; Set fill column to 88 instead of 70.
  (setq-default fill-column 88)

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

  ;; Allow setting the goal column.
  (put 'set-goal-column 'disabled nil)

  ;; Place the something~ backup files in the temp dir.
  (setq backup-directory-alist
            `((".*" . ,temporary-file-directory)))

  ;; Close the' ctrl-c b/t' beautify/test buffers when succesful.
  (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

  :bind (("C-c s" . sort-lines)
         )
  )

(provide 'init.el)
;;; init.el ends here
