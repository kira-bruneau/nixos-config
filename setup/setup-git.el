(require-package
 '(git-gutter-fringe
   gitconfig-mode
   gitignore-mode
   magit-filenotify
   magit))

(require-binary
 '(git))

(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c C-v") 'magit-status)
(global-git-gutter-mode t)

(add-hook 'magit-status-mode-hook
          (lambda ()
            (magit-filenotify-mode t)))

(provide 'setup-git)
