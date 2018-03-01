(straight-use-package 'adaptive-wrap)
(straight-use-package 'doom-themes)
(straight-use-package 'powerline)

;; Theme
(setq doom-vibrant-padded-modeline 8)
(doom-themes-org-config)

;; Fix blue modeline using emacsclient with doom theme
;; Source: https://github.com/hlissner/emacs-doom-themes/issues/125
(defun doom|init-theme ()
  "Set the theme and load the font, in that order."
  (load-theme 'doom-vibrant t)
  (add-hook 'after-make-frame-functions #'doom|init-theme-in-frame))

(defun doom|init-theme-in-frame (frame)
  "Reloads the theme in new daemon or tty frames."
  (when (or (daemonp) (not (display-graphic-p)))
    (with-selected-frame frame
      (doom|init-theme))))

(add-hook 'emacs-startup-hook #'doom|init-theme)

;; Font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 9"))

;; Powerline mode
(powerline-default-theme)

;; GUI cleanup
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Visual line mode
(set-default 'truncate-lines t)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(diminish 'visual-line-mode "↩")

;; Scroll one line at a time (less "jumpy" than defaults)
;; Source: http://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

;; Misc
(setq-default tab-width 4)
(setq column-number-mode t)
(show-paren-mode t)
(setq-default word-wrap t)
