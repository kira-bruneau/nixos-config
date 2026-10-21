(use-package wgsl-ts-mode
  :mode "\\.wgsl\\'"
  :init
  (with-eval-after-load 'lsp-bridge
    (add-to-list 'lsp-bridge-single-lang-server-mode-list '(wgsl-ts-mode . "wgsl"))
    (add-to-list 'lsp-bridge-default-mode-hooks 'wgsl-ts-mode-hook)
    (add-to-list 'lsp-bridge-formatting-indent-alist '(wgsl-ts-mode . wgsl-ts-mode-indent-offset))))
