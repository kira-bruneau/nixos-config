(use-package python
  :mode ("\\.py[iw]?\\'" . python-ts-mode)
  :interpreter ("python[0-9.]*" . python-ts-mode)

  :config
  (setq lsp-pyls-plugins-pycodestyle-max-line-length 99))
