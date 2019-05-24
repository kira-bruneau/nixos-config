(use-package elpy
  :straight t
  :ensure-system-package
  (;; Arch Linux
   (autopep8 . autopep8)
   (flake8 . flake8)
   (yapf . yapf)

   ;; NixOS
   (autopep8 . python3.7-autopep8)
   (flake8 . python3.7-flake8)
   (yapf . python3.7-yapf))

  :hook (elpy-mode . flycheck-mode)
  :init (elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))
