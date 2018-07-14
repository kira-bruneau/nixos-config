(use-package typescript-mode
  :straight t)

(use-package tide
  :straight t
  :after typescript-mode
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (tide-mode . flycheck-mode)
         (tide-mode . company-mode)
         (tide-mode . eldoc-mode)))
