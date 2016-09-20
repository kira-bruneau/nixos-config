(require-package
 '(git-timemachine
   gitconfig-mode
   gitignore-mode
   magit))

(require-binary
 '(git))

;; Magit
(global-set-key (kbd "M-<f12>") 'magit-status)
(setq magit-save-repository-buffers nil)

(provide 'setup-git)
