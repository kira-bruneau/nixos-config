(require-package
 '(multiple-cursors))

(setq mc/list-file (concat dir/cache ".mc-lists.el"))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-^") 'mc/mark-all-like-this)
(global-set-key (kbd "C-'") 'mc-hide-unmatched-lines-mode)

(provide 'setup-multiple-cursors)
