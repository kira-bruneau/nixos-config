(use-package python
  :ensure-system-package
  (;; NixOS
   (pyls . python3.7-python-language-server) ;; language server
   (pyflakes . python3.7-pyflakes)           ;; error linter
   (pycodestyle . python3.7-pycodestyle)     ;; style linter
   (yapf . python3.7-yapf)                   ;; code formatting
   ;; (??? . python3.7-rope)                 ;; completions and renaming

   ;; Arch Linux
   (pyls . python-language-server)
   (pyflakes . python-pyflakes)
   (pycodestyle . python-pycodestyle)
   (yapf . yapf)
   ;; (??? . python-rope)
   )
  :hook (python-mode . lsp))
