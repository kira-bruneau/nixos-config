(use-package cider
  :straight t
  :defer t
  :hook ((cider-mode . company-mode)
         (cider-repl-mode . company-mode)))
