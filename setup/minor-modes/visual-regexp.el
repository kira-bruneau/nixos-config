(use-package visual-regexp
  :straight t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

(use-package visual-regexp-steroids
  :straight t)
