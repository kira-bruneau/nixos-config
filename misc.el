(straight-use-package 'diminish)

;; Default configuration
(global-auto-revert-mode t)
(setq-default indent-tabs-mode nil)
(set-default 'truncate-lines t)
(delete-selection-mode t)
(setq shift-select-mode nil)

;; Minor modes
(winner-mode t)
(windmove-default-keybindings)
(savehist-mode t)

(global-set-key (kbd "C-c C-r") 'revert-buffer)
