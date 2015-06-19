;; TODO: Package dependency management

;; == Initialization == ;;
(add-to-list 'load-path (concat user-emacs-directory "setup/"))

;; Init
(require 'setup-packages)
(require 'setup-ui)

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

;; Other
(require 'setup-misc)
(require 'setup-custom)
