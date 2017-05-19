(require-package
 '(adaptive-wrap
   doom-themes
   hl-line+
   powerline))

;; Theme
(setq doom-one-padded-modeline 8)
(load-theme 'doom-one t)

;; Brighten thangs
;; (add-hook 'find-file-hook #'doom-buffer-mode-maybe)
;; (add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
;; (add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)

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

;; Misc
(setq-default tab-width 4)
(setq column-number-mode t)
(show-paren-mode t)
(setq-default word-wrap t)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
;; (setq-default cursor-type '(bar . 2)) ;; 2px since I have a high dpi monitor
(blink-cursor-mode t)
