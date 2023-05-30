(use-package nix-mode
  :init
  (with-eval-after-load 'aphelia
    (add-to-list 'apheleia-formatters '((nixpkgs-fmt "nixpkgs-fmt")))
    (add-to-list 'apheleia-mode-alist '((nix-mode nixpkgs-fmt))))

  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                    :major-modes '(nix-mode)
                    :server-id 'nixd)))
