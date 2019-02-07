;; Simplify all yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Push clipboard to the kill ring
(setq save-interprogram-paste-before-kill t)

;; Don't ask to create a new file
(setq confirm-nonexistent-file-or-buffer nil)

;; Don't prompt to kill a buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Don't prompt to revert a buffer
(setq revert-without-query '(".*"))

;; Stop giving me annoying tramp messages
(setq tramp-verbose 2)

;; Stop putting custom config in my init.el
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Turn off the annoying bell
(setq ring-bell-function 'ignore)

;; Disable warning about running lisp code in theme
(setq custom-safe-themes t)

;; Don't prompt to follow git symlink
(setq vc-follow-symlinks t)

;; Disable bidirectional reordering to improve performance of file with long lines
(setq-default bidi-display-reordering nil)

;; Store all backup files in a single directory
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/"))))
