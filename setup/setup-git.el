(require-package
 '(git-timemachine
   gitconfig-mode
   gitignore-mode
   magit))

(require-binary
 '(git))

;; Magit
(setq magit-save-repository-buffers nil)
(global-set-key (kbd "M-<f12>") 'magit-status)

(provide 'setup-git)
