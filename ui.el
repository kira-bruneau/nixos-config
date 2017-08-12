(straight-use-package 'adaptive-wrap)
(straight-use-package 'doom-themes)
(straight-use-package 'hl-line+)
(straight-use-package 'powerline)

;; Theme
(setq doom-one-padded-modeline 8)
(load-theme 'doom-one t)

;; Font
(set-frame-font "Inconsolata 12" nil t)

;; Powerline mode
(powerline-default-theme)

;; GUI cleanup
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Start frames maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Visual line mode
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(diminish 'visual-line-mode "↩")

;; Misc
(setq-default tab-width 4)
(setq column-number-mode t)
(show-paren-mode t)
(setq-default word-wrap t)
