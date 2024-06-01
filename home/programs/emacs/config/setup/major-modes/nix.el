(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :init
  (with-eval-after-load 'lsp-mode
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                      :major-modes '(nix-ts-mode)
                      :server-id 'nixd))))
