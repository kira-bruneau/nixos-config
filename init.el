;; Init
(package-initialize)

(setq dir/setup (concat user-emacs-directory "setup/")
      dir/cache (concat user-emacs-directory "cache/")
      dir/conf (concat user-emacs-directory "conf/"))

(add-to-list 'load-path dir/setup)

(require 'setup-dependencies)
(require 'setup-ui)
(require 'setup-misc)

;; Major Modes
(require 'language-arduino)
(require 'language-c)
(require 'language-go)
(require 'language-javascript)
(require 'language-latex)
(require 'language-lisp)
(require 'language-php)
(require 'language-rust)
(require 'language-sql)
(require 'language-typescript)
(require 'setup-dired)
(require 'setup-ediff)
(require 'setup-org)

;; Minor Modes
(require 'setup-buffer-move)
(require 'setup-company)
(require 'setup-doc-view)
(require 'setup-drag-stuff)
(require 'setup-flycheck)
(require 'setup-flyspell)
(require 'setup-ggtags)
(require 'setup-git)
(require 'setup-helm)
(require 'setup-ido)
(require 'setup-multi-term)
(require 'setup-multiple-cursors)
(require 'setup-projectile)
(require 'setup-rainbow-delimiters)
(require 'setup-smartparens)
(require 'setup-undo-tree)
(require 'setup-visual-regexp)
(require 'setup-yasnippet)

;; Custom
(require 'setup-astyle)
(require 'setup-sudo-edit)
