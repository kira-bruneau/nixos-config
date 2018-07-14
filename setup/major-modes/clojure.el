(use-package cider
  :straight t
  :hook ((cider-mode . company-mode)
         (cider-repl-mode . company-mode)))
