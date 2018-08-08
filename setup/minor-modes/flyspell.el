(use-package flyspell
  :diminish " ≈"
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :bind (("C-S-<f8>" . flyspell-mode)))
