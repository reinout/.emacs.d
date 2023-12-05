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
(defvar my-packages '(better-defaults
		      exec-path-from-shell
		      magit
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


(provide 'init.el)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(modus-themes magit exec-path-from-shell better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
