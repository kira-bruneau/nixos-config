(use-package direnv
  :straight t
  :init
  (direnv-mode)

  :config
  (setq direnv-always-show-summary nil))
