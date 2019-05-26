(use-package php-mode
  :straight t
  :ensure-system-package
  ((npm . npm)
   (npm . nodejs)
   (intelephense . "npm i -g intelephense"))
  :hook (php-mode . lsp))
