(use-package markdown-mode
  :straight t
  :ensure-system-package pandoc
  :config
  (setq markdown-command "pandoc"))
