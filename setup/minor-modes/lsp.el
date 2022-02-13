(use-package lsp-mode
  :straight t
  :hook ((c++-mode . lsp)
         (c-mode . lsp)
         (cmake-mode . lsp)
         (csharp-mode . lsp)
         (go-mode . lsp)
         (java-mode . lsp)
         (js2-mode . lsp)
         (latex-mode . lsp)
         (objc-mode . lsp)
         (php-mode . lsp)
         (python-mode . lsp)
         (ruby-mode . lsp)
         (rust-mode . lsp)
         (sh-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp))
  :bind-keymap (("<f6>" . lsp-command-map))
  :bind (:map lsp-mode-map ("C-r" . lsp-rename))
  :config
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-file-watchers nil) ;; file watchers cause emacs to hang on large projects
  (setq lsp-enable-snippet nil)
  (setq lsp-response-timeout 1.0e+INF) ;; can always ctrl+g my way out of this
  (setq lsp--show-message nil)

  ;; Use typescript-language-server & tsserver from PATH
  (lsp-dependency 'typescript-language-server `(:system ,(executable-find "typescript-language-server")))
  (lsp-dependency 'typescript `(:system ,(executable-find "tsserver"))))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("M-." . lsp-ui-peek-find-definitions)
              ("C-M-." . lsp-find-definition)
              ("M-/" . lsp-ui-peek-find-references)
              ("C-M-/" . lsp-find-references)
              ("M-?" . lsp-ui-doc-mode)
              ("C-M-?" . lsp-ui-doc-enable-focus-frame))
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-doc-max-width 80)
  (setq lsp-ui-doc-border (doom-color 'vertical-bar))

  (defun lsp-ui-doc-enable-focus-frame ()
    (interactive)
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-show)
    (lsp-ui-doc-focus-frame)))
