(require 'lsp-mode)

(use-package latex-mode
  :bind (:map latex-mode-map
              ("M-p" . latex-preview-pane-mode)))

(use-package latex-preview-pane
  :straight t)
