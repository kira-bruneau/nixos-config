(use-package magit
  :straight (magit :type git :host github :repo "MetaDark/magit"
                   :upstream (:host github :repo "magit/magit"))
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
