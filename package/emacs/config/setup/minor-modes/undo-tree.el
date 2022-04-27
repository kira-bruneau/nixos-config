(use-package undo-tree
  :straight t
  :diminish "" ;; ψ
  :init
  (global-undo-tree-mode t)

  :config
  (setq undo-tree-history-directory-alist `(("." . ,dir/aux))))
