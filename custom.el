(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(package-selected-packages
   '(ag arduino-mode better-defaults buffer-env company deadgrep direnv dockerfile-mode
        eglot-ltex-plus exec-path-from-shell flycheck-languagetool flymake-languagetool
        forge git-commit lsp-mode lsp-pyright lsp-ui magit magit-section mermaid-mode
        modus-themes pdf-tools pet pkg-info projectile python-pytest ripgrep
        terraform-mode transient unfill visual-fill-column yaml-mode yaml-pro
        yasnippet-snippets))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "make install")
     (projectile-project-test-cmd . "make test")
     (projectile-project-compilation-cmd . "make")
     (projectile-project-test-cmd . "bin/pytest")
     (projectile-project-compilation-cmd . "bin/pip install -r requirements.txt")
     (lsp-pyright-python-venv-path . "~/zelf/advent-2023/")
     (lsp-pyright-python-executable-cmd . "bin/python")
     (python-pytest-executable . "bin/pytest")
     (projectile-project-test-cmd . "docker compose run --rm web make test")
     (projectile-project-compilation-cmd . "docker compose build")))
 '(split-height-threshold 200)
 '(use-package-enable-imenu-support t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
