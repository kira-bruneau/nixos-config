(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-^" . mc/mark-all-like-this))
  :init
  (setq mc/list-file (concat dir/cache "mc-lists.el"))
  (with-eval-after-load 'multiple-cursors-core
    (define-key mc/keymap (kbd "M-T") 'mc/reverse-regions)
    (define-key mc/keymap (kbd "C-,") 'mc/skip-to-previous-like-this)
    (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this)))
