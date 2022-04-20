(use-package autorevert
  :bind (("C-c C-r" . revert-buffer))
  :config
  (setq auto-revert-verbose nil)
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode t))
