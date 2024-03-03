(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :init
  (with-eval-after-load 'aphelia
    (add-to-list 'apheleia-formatters '(nixpkgs-fmt . "nixpkgs-fmt"))
    (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixpkgs-fmt)))

  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(nix-ts-mode . "nix"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                      :major-modes '(nix-ts-mode)
                      :server-id 'nixd))))
