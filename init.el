;; Init
(setq dir/setup (concat user-emacs-directory "setup/")
      dir/cache (concat user-emacs-directory "cache/")
      dir/conf (concat user-emacs-directory "conf/"))

(add-to-list 'load-path dir/setup)

(require 'setup-packages)
(require 'setup-ui)
(require 'setup-misc)

;; Generic helpers
(require 'setup-tagging)
(require 'setup-snippets)
(require 'setup-linting)
(require 'setup-spelling)

;; Minor modes
(require 'setup-buffer-move)
(require 'setup-company)
(require 'setup-diminish)
(require 'setup-git)
(require 'setup-ido)
(require 'setup-multiple-cursors)
(require 'setup-projectile)
(require 'setup-smartparens)
(require 'setup-undo-tree)
(require 'setup-visual-regexp)

;; Major modes
(require 'setup-org)
(require 'setup-ediff)
(require 'language-c)
(require 'language-javascript)
(require 'language-latex)
(require 'language-lisp)
(require 'language-php)
(require 'language-sql)
