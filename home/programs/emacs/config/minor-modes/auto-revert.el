(use-package autorevert
  :bind* ("<f5>" . revert-buffer)
  :hook (dired-mode . auto-revert-mode)
  :config
  (setq auto-revert-verbose nil)
  (setq auto-revert-avoid-polling t)
  (global-auto-revert-mode))
