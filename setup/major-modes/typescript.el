(use-package typescript-mode
  :straight t)

(use-package tide
  :straight t
  :after typescript-mode
  :hook
  ((typescript-mode . (lambda ()
                        (tide-setup)
                        (tide-hl-identifier-mode)))
   (tide-mode . (lambda ()
                  (flycheck-mode)
                  (company-mode)
                  (eldoc-mode)))))
