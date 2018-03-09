(use-package magit
  :straight t
  ;; :ensure-system-package git
  :defer t
  :config
  (setq magit-save-repository-buffers nil))

(use-package git-timemachine
  :straight t
  :defer t)

(use-package gitconfig-mode
  :straight t
  :defer t)

(use-package gitignore-mode
  :straight t
  :defer t)
