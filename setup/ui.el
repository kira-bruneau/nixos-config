;; GUI cleanup
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Theme
(use-package doom-themes
  :straight t
  :config
  (setq doom-vibrant-padded-modeline 8) ;; 2x original value for high dpi scaling
  (doom-themes-org-config)
  (load-theme 'doom-vibrant t))

(use-package powerline
  :straight t
  :config
  (powerline-default-theme))

;; Font (current frame + future frames)
(let ((font "DejaVu Sans Mono 9"))
  (set-frame-font font)
  (add-to-list 'default-frame-alist (cons 'font font)))

;; Buffer settings
(setq-default tab-width 4)

;; Line wrapping + misc
(use-package simple
  :demand t
  :diminish (visual-line-mode . " ↩")
  :bind (("C-c v" . 'visual-line-mode))
  :config
  (setq column-number-mode t)
  (global-visual-line-mode))

(use-package adaptive-wrap
  :straight t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

;; Highlight matching parenthesis
(use-package paren
  :demand t
  :config
  (show-paren-mode t))

;; Finer mouse/trackpad scrolling
;; Source: http://www.emacswiki.org/emacs/SmoothScrolling
(use-package mwheel
  :demand t
  :config
  ;; (setq scroll-step 1)
  ;; (setq scroll-conservatively 100)
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil))
