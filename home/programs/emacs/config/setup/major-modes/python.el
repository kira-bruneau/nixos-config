(use-package python
  :mode ("\\.py[iw]?\\'" . python-ts-mode)
  :interpreter ("python[0-9.]*" . python-ts-mode)
  :init
  (with-eval-after-load 'lsp-bridge
    (setq lsp-bridge-python-lsp-server "pylsp")))
