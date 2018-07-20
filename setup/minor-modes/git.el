(use-package magit
  :straight (magit :type git :host github :repo "MetaDark/magit"
                   :upstream (:host github :repo "magit/magit"))
  :ensure-system-package git
  :bind ("C-c C-b" . magit-blame)
  :config
  (setq magit-save-repository-buffers nil))

(use-package git-timemachine
  :straight t
  :bind ("C-c C-t" . git-timemachine))

(use-package browse-at-remote
  :straight t
  :bind ("C-c o" . browse-at-remote))




