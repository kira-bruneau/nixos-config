(straight-use-package 'git-timemachine)
(straight-use-package 'gitconfig-mode)
(straight-use-package 'gitignore-mode)
(straight-use-package 'magit)

(pacaur-use-packages
 '(git))

;; Magit
(global-set-key (kbd "M-<f12>") 'magit-status)
(setq magit-save-repository-buffers nil)
