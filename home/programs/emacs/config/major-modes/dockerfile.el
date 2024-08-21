(use-package dockerfile-ts-mode
  :mode "[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'"
  :init
  (with-eval-after-load 'lsp-bridge
    (add-to-list 'lsp-bridge-single-lang-server-mode-list '(dockerfile-ts-mode . "docker-langserver"))
    (add-to-list 'lsp-bridge-default-mode-hooks 'dockerfile-ts-mode-hook)))
