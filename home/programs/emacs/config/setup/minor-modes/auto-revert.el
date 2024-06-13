(use-package autorevert
  :bind (("C-c C-r" . revert-buffer))
  :config
  (setq auto-revert-verbose nil)
  (setq auto-revert-avoid-polling t)
  (global-auto-revert-mode t))
