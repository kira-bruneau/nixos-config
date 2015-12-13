(require-package
 '(git-gutter-fringe
   git-timemachine
   gitconfig-mode
   gitignore-mode
   magit))

(require-binary
 '(git))

;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil)
(global-set-key (kbd "M-<f12>") 'magit-status)

(provide 'setup-git)
