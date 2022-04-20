(use-package csharp-mode
  :straight t
  :config
  (setq lsp-csharp-server-path (executable-find "omnisharp")))
