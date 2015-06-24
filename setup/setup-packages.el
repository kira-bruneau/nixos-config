(setq package-list
      '(lua-mode
        markdown-mode
        paradox
        pkgbuild-mode
        rust-mode))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Refresh the archive
(unless package-archive-contents
  (package-refresh-contents))

(defun package-require (packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(defun extern-require (externs)
  (message (concat
            "sudo pacman -S --needed "
            (mapconcat 'symbol-name externs " "))))

(package-require package-list)

(provide 'setup-packages)
