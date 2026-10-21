(use-package java-ts-mode
  :hook (java-ts-mode . (lambda () (lsp-bridge-semantic-tokens-mode -1))))
