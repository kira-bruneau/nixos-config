(use-package rust-mode
  :straight t
  :ensure-system-package rls
  :hook (rust-mode . lsp))
