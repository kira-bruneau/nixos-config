(use-package rust-mode
  :straight t
  :ensure-system-package rust-analyzer
  :hook (rust-mode . lsp))
