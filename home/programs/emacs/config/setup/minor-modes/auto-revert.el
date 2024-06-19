(use-package autorevert
  :bind (("<f5>" . revert-buffer))
  :config
  (setq auto-revert-verbose nil)
  (setq auto-revert-avoid-polling t)
  (global-auto-revert-mode t))
