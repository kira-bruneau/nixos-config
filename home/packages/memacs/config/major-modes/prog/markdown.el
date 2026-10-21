(use-package markdown-mode
  :custom
  (markdown-command "pandoc")

  :init
  (with-eval-after-load 'lsp-bridge
    (setopt lsp-bridge-markdown-lsp-server "marksman")))
