(use-package lsp-mode
  :straight t
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-auto-guess-root t))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-mode-map
              ("C-r" . lsp-rename)
         :map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-flycheck-enable t))
