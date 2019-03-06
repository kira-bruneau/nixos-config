(use-package lsp-mode
  :straight t)

(use-package lsp-ui
  :straight t
  :hook ((lsp-mode . lsp-ui-mode))
  :config
  (setq lsp-ui-sideline-enable nil))
