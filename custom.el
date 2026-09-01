;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(eglot-autoshutdown t)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((web-mode-indent-style . 2) (web-mode-block-padding . 2) (web-mode-script-padding . 2)
     (web-mode-style-padding . 2) (projectile-project-compilation-cmd . "make install")
     (projectile-project-test-cmd . "make test")
     (projectile-project-compilation-cmd . "make")
     (projectile-project-test-cmd . "bin/pytest")
     (projectile-project-compilation-cmd . "bin/pip install -r requirements.txt")
     (lsp-pyright-python-venv-path . "~/zelf/advent-2023/")
     (lsp-pyright-python-executable-cmd . "bin/python")
     (python-pytest-executable . "bin/pytest")
     (projectile-project-test-cmd . "docker compose run --rm web make test")
     (projectile-project-compilation-cmd . "docker compose build")))
 '(sentence-end-double-space nil)
 '(split-height-threshold 200)
 '(use-package-enable-imenu-support t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
