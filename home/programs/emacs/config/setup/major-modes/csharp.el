(use-package csharp-ts-mode
  :mode "\\.cs\\'"
  :config
  (setq lsp-csharp-server-path (executable-find "omnisharp")))
