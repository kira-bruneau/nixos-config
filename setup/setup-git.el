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
(setq magit-status-sections-hook
      (remove-duplicates
       (append
        magit-status-sections-hook
        '(magit-insert-recent-commits))
       :test 'eq))

(provide 'setup-git)
