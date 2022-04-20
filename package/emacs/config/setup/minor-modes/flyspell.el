(use-package flyspell
  :diminish " ≈"
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :bind (("C-S-<f5>" . flyspell-mode)))
