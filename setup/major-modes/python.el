(use-package elpy
  :straight t
  :ensure-system-package
  ((python . python)
   (autopep8 . "pip install autopep8")
   (flake8 . "pip install flake8")
   (yapf . "pip install yapf"))
  :hook ((elpy-mode . flycheck-mode))
  :init (elpy-enable))
