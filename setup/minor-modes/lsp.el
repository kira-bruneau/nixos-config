(use-package lsp-mode
  :straight t)

(use-package lsp-ui
  :straight t
  :hook ((lsp-mode . lsp-ui-mode))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-sideline-enable nil))
