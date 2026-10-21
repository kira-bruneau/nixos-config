(use-package autorevert
  :bind* ("<f5>" . revert-buffer)
  :hook (dired-mode . auto-revert-mode)

  :custom
  (auto-revert-verbose nil)
  (auto-revert-avoid-polling t)

  :config
  (global-auto-revert-mode))
