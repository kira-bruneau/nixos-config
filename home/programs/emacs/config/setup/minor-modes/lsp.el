(use-package lsp-mode
  :hook ((c++-ts-mode . lsp)
         (c-ts-mode . lsp)
         (cmake-ts-mode . lsp)
         (csharp-ts-mode . lsp)
         (go-ts-mode . lsp)
         (java-ts-mode . lsp)
         (js-ts-mode . lsp)
         (latex-mode . lsp)
         (nix-ts-mode . lsp)
         (objc-mode . lsp)
         (php-mode . lsp)
         (python-ts-mode . lsp)
         (ruby-ts-mode . lsp)
         (rust-ts-mode . lsp)
         (bash-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (vala-mode . lsp)
         (web-mode . lsp))
  :bind-keymap (("<f6>" . lsp-command-map))
  :config
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-file-watchers nil) ;; file watchers cause emacs to hang on large projects
  (setq lsp-enable-snippet nil)
  (setq lsp-response-timeout 1.0e+INF) ;; can always ctrl+g my way out of this
  (setq lsp--show-message nil)
  (setq lsp-idle-delay 0)
  (setq lsp-auto-execute-action nil)
  (setq lsp-lens-enable nil) ;; lenses are really laggy right now, re-enable this if the lag is ever fixed
  (setq lsp-headerline-breadcrumb-enable nil) ;; headerline laggy with pixel precision scrolling

  ;; Use typescript-language-server & tsserver from PATH
  (lsp-dependency 'typescript-language-server `(:system ,(executable-find "typescript-language-server")))
  (lsp-dependency 'typescript `(:system ,(executable-find "tsserver"))))

(use-package lsp-ui
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
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-delay 0)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-doc-max-width 80)
  (setq lsp-ui-doc-border (doom-color 'vertical-bar))

  (defun lsp-ui-doc-enable-focus-frame ()
    (interactive)
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-show)
    (lsp-ui-doc-focus-frame)))
