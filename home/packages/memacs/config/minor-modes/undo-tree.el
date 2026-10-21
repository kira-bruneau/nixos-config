(use-package undo-tree :demand
  :custom
  (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "aux"))))

  :config
  (global-undo-tree-mode))
