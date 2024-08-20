(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :init
  (with-eval-after-load 'lsp-bridge
    (setq lsp-bridge-nix-lsp-server "nixd")
    (add-to-list 'lsp-bridge-single-lang-server-mode-list '(nix-ts-mode . lsp-bridge-nix-lsp-server))
    (add-to-list 'lsp-bridge-default-mode-hooks 'nix-ts-mode-hook)
    (add-to-list 'lsp-bridge-formatting-indent-alist '(nix-ts-mode . nix-ts-mode-indent-offset))))
