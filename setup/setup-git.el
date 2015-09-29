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
(global-set-key (kbd "M-<f12>") 'magit-status)

;; Set custom order for magit-status
(setq magit-status-sections-hook
      '(magit-insert-status-headers
        magit-insert-merge-log
        magit-insert-rebase-sequence
        magit-insert-am-sequence
        magit-insert-sequencer-sequence
        magit-insert-bisect-output
        magit-insert-bisect-rest
        magit-insert-bisect-log
        magit-insert-stashes
        magit-insert-staged-changes
        magit-insert-unstaged-changes
        magit-insert-untracked-files
        magit-insert-unpulled-commits
        magit-insert-unpushed-commits))

(global-git-gutter-mode t)

(provide 'setup-git)
