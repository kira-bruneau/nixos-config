(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-list
      '(adaptive-wrap
        ag
        auctex
        buffer-move
        company
        ;; company-quickhelp
        company-tern
        ;; diminish
        drag-stuff
        dtrt-indent
        elisp-slime-nav
        expand-region
        flx
        flx-ido
        flx-isearch
        flycheck
        ggtags
        git-gutter-fringe
        gitconfig-mode
        gitignore-mode
        ido-ubiquitous
        ido-vertical-mode
        js2-mode
        latex-preview-pane
        lua-mode
        paradox
        magit
        monokai-theme
        multiple-cursors
        php-mode
        pkgbuild-mode
        powerline
        projectile
        rust-mode
        slime
        smartparens
        smex
        tern
        sql-indent
        undo-tree
        visual-regexp
        visual-regexp-steroids
        web-mode
        whitespace-cleanup-mode))

(package-initialize)

;; Refresh the archive
(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'setup-packages)
