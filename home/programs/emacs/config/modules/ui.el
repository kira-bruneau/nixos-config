;; GUI cleanup
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setopt inhibit-startup-screen t)
(setopt initial-major-mode 'fundamental-mode)
(setopt initial-scratch-message nil)

;; Maximize window
(setq ns-auto-hide-menu-bar t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Theme
(use-package doom-themes :demand
  :custom
  (doom-themes-padded-modeline 3)
  (org-todo-keyword-faces
   `(("TODO" :foreground ,(doom-color 'green) :weight bold)
     ("INPROGRESS" :foreground ,(doom-color 'blue) :weight bold)
     ("WAITING" :foreground ,(doom-color 'yellow) :weight bold)
     ("DONE" :foreground ,(doom-color 'grey)  :weight bold)
     ("CANCELLED" :foreground ,(doom-color 'grey) :weight bold)))

  :config
  (load-theme 'doom-vibrant t))

(use-package powerline :demand
  :custom
  (powerline-display-buffer-size nil)

  :config
  (defun powerline-minimal-theme ()
    "Setup the minimal mode-line."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (vc-mode-line (string-trim (format-mode-line '(vc-mode vc-mode))))
                            (lhs (list (when powerline-display-buffer-size
                                         (powerline-buffer-size face0 'l))
                                       (when powerline-display-mule-info
                                         (powerline-raw mode-line-mule-info face0 'l))
                                       (powerline-raw "%*" face0)
                                       (when (and (boundp 'which-func-mode) which-func-mode)
                                         (powerline-raw which-func-format face0 'l))
                                       (funcall separator-left face0 face1)
                                       (powerline-raw "%b" face1 'l)
                                       (powerline-raw " " face1)
                                       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                         (powerline-raw erc-modified-channels-object face1 'l))
                                       (powerline-narrow face1 'l)
                                       (funcall separator-left face1 face2)
                                       (unless (string-empty-p vc-mode-line)
                                         (powerline-raw (concat "(" vc-mode-line ")") face2 'l))))
                            (rhs (list (powerline-raw global-mode-string face2 'r)
                                       (funcall separator-right face2 face1)
                                       (unless window-system
                                         (powerline-raw (char-to-string #xe0a1) face1 'l))
                                       (powerline-major-mode face1 'l)
                                       (powerline-raw " " face1)
                                       (funcall separator-right face1 face0)
                                       (powerline-raw "%l : %c" face0 'l)
                                       (powerline-raw " " face0)
                                       (when powerline-display-hud
                                         (powerline-hud face2 face1))
                                       (powerline-fill face0 0)
                                       )))
                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs)))))))

  (powerline-minimal-theme))

;; Font (current frame + future frames)
(let ((font (if (eq system-type 'darwin) "Monaco 12" "DejaVu Sans Mono 9")))
  (set-frame-font font)
  (add-to-list 'default-frame-alist (cons 'font font)))

;; Buffer settings
(setq-default tab-width 2)

;; Line wrapping + misc
(use-package simple
  :bind* (("C-c v" . visual-line-mode))
  :config
  (column-number-mode)
  (set-default 'truncate-lines t))

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

;; Highlight matching parenthesis
(use-package paren
  :config
  (show-paren-mode))

;; Pixel precision scrolling
(use-package pixel-scroll :demand
  :custom
  (pixel-scroll-precision-use-momentum t)
  (pixel-scroll-precision-initial-velocity-factor 0.02)
  (pixel-scroll-precision-momentum-seconds 1.0)

  :config
  (pixel-scroll-precision-mode))
