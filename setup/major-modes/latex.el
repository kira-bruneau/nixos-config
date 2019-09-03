(require 'lsp-mode)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "/tmp/nix/texlab/target/release/texlab")
                  :major-modes '(latex-mode bibtex-mode)
                  :server-id 'texlab))

(add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))
(add-to-list 'lsp-language-id-configuration '(bibtex-mode . "bibtex"))

(use-package latex-mode
  :bind (:map latex-mode-map
              ("M-p" . latex-preview-pane-mode))
  :hook (latex-mode . lsp))

(use-package bibtex-mode
  :hook (bibtex-mode . lsp))

(use-package latex-preview-pane
  :straight t
  :defer t)
