(setq package-list
      '(diminish))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Refresh the archive
(unless package-archive-contents
  (package-refresh-contents))

(defun require-package (packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(defun require-binary (externs)
  ;; (message (concat
  ;;           "sudo pacman -S --needed "
  ;;           (mapconcat 'symbol-name externs " ")))
  )

(require-package package-list)

(provide 'setup-dependencies)
