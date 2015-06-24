(package-require
 '(multiple-cursors))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-^") 'mc/mark-all-like-this)
(global-set-key (kbd "C-'") 'mc-hide-unmatched-lines-mode)

(provide 'setup-multiple-cursors)
