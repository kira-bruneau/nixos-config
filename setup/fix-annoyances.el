(defalias 'yes-or-no-p 'y-or-n-p) ;; Simplify all yes/no to y/n
(setq save-interprogram-paste-before-kill t) ;; Push clipboard to the kill ring
(setq confirm-nonexistent-file-or-buffer nil) ;; Don't ask to create a new file
(global-set-key (kbd "C-c C-k") 'kill-this-buffer) ;; Don't prompt to kill a buffer
(setq revert-without-query '(".*")) ;; Don't prompt to revert a buffer
(setq tramp-verbose 2) ;; Stop giving me annoying tramp messages
(setq ad-redefinition-action 'accept) ;; Stop warning `ido-completing-read' got redefined
(setq custom-file (concat user-emacs-directory "custom.el")) ;; Stop putting custom config in my init.el
(setq ring-bell-function 'ignore) ;; Turn off the annoying bell
(setq custom-safe-themes t) ;; Disable warning about running lisp code in theme
