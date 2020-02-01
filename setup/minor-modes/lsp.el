(use-package lsp-mode
  :straight t
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-file-watchers nil)) ;; file watchers cause emacs to hang on large projects

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-mode-map
              ("C-r" . lsp-rename)
         :map lsp-ui-mode-map
              ("M-." . lsp-ui-peek-find-definitions)
              ("C-M-." . lsp-find-definition)
              ("M-?" . lsp-ui-peek-find-references)
              ("C-M-?" . lsp-find-references))
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-flycheck-enable t))
