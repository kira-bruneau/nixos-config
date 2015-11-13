(require-package
 '(multiple-cursors))

(setq mc/list-file (concat dir/cache "mc-lists.el"))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-^") 'mc/mark-all-like-this)
(global-set-key (kbd "C-'") 'mc-hide-unmatched-lines-mode)

(with-eval-after-load 'multiple-cursors-core
  (define-key mc/keymap (kbd "M-T") 'mc/reverse-regions)
  (define-key mc/keymap (kbd "C-,") 'mc/skip-to-previous-like-this)
  (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this))

(provide 'setup-multiple-cursors)
