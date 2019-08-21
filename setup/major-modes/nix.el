(use-package nix-mode
  :straight t
  :hook (nix-mode . (lambda () (when (executable-find "nix-lsp") (lsp))))
  :config
  (require 'lsp-mode)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nix-lsp")
                    :major-modes '(nix-mode)
                    :server-id 'nix-lsp))

  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix")))
