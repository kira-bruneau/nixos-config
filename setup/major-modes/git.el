(use-package magit
  :straight (magit :type git :host github :repo "MetaDark/magit"
                   :upstream (:host github :repo "magit/magit"))
  :ensure-system-package git
  :config
  (setq magit-save-repository-buffers nil))

(use-package git-timemachine
  :straight t)

(use-package gitconfig-mode
  :straight t)

(use-package gitignore-mode
  :straight t)
