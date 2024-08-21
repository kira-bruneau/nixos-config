(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-ts-mode)
  :init
  (with-eval-after-load 'lsp-bridge
    (add-to-list 'lsp-bridge-single-lang-server-mode-list '(csharp-ts-mode . lsp-bridge-csharp-lsp-server))
    (add-to-list 'lsp-bridge-default-mode-hooks 'csharp-ts-mode-hook)
    (add-to-list 'lsp-bridge-formatting-indent-alist '(csharp-ts-mode . csharp-ts-mode-indent-offset))))
