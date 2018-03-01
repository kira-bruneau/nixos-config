(straight-use-package 'page-break-lines)

(dolist (mode '(c-mode magit-mode))
  (add-to-list 'page-break-lines-modes mode))

(global-page-break-lines-mode)
