;; Init
(setq dir/setup (concat user-emacs-directory "setup/")
      dir/cache (concat user-emacs-directory "cache/")
      dir/conf (concat user-emacs-directory "conf/"))

(add-to-list 'load-path dir/setup)

(require 'setup-packages)
(require 'setup-ui)
(require 'setup-misc)

;; Generic helpers
(require 'setup-linting)
(require 'setup-snippets)
(require 'setup-spelling)
(require 'setup-tagging)

;; Minor modes
(require 'setup-astyle)
(require 'setup-buffer-move)
(require 'setup-company)
(require 'setup-drag-stuff)
(require 'setup-git)
(require 'setup-helm)
(require 'setup-ido)
(require 'setup-multiple-cursors)
(require 'setup-projectile)
(require 'setup-rainbow-delimiters)
(require 'setup-smartparens)
(require 'setup-sudo-edit)
(require 'setup-undo-tree)
(require 'setup-visual-regexp)

;; Major modes
(require 'setup-dired)
(require 'setup-ediff)
(require 'setup-org)
(require 'setup-multi-term)
(require 'language-c)
(require 'language-go)
(require 'language-javascript)
(require 'language-latex)
(require 'language-lisp)
(require 'language-php)
(require 'language-rust)
(require 'language-sql)
