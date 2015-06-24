(package-require
 '(git-gutter-fringe
   gitconfig-mode
   gitignore-mode
   magit-filenotify
   magit))

(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c C-v") 'magit-status)
(global-git-gutter-mode t)

(provide 'setup-git)
