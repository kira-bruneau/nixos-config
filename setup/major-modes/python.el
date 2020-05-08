(use-package python
  ;; Also ensure "rope" module is installed for completion and renaming
  :ensure-system-package
  (;; NixOS
   (pyls . python3.7-python-language-server) ;; language server
   (pyflakes . python3.7-pyflakes)           ;; error linter
   (pycodestyle . python3.7-pycodestyle)     ;; style linter
   (yapf . python3.7-yapf)                   ;; code formatting

   ;; Arch Linux
   (pyls . python-language-server)
   (pyflakes . python-pyflakes)
   (pycodestyle . python-pycodestyle)
   (yapf . yapf))
  :config
  (setq lsp-pyls-plugins-pycodestyle-max-line-length 99))
