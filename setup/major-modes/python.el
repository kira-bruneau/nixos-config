(use-package elpy
  :straight t
  :ensure-system-package
  ((flake8 . flake8)
   (autopep8 . autopep8))
  :hook ((elpy-mode . flycheck-mode))
  :init (elpy-enable))
