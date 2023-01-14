(use-package page-break-lines
  :init
  (require 'page-break-lines)
  (dolist (mode '(c-mode magit-mode))
    (add-to-list 'page-break-lines-modes mode))

  (global-page-break-lines-mode))
