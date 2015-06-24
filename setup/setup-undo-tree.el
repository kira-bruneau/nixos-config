(package-require
 '(undo-tree))

(global-undo-tree-mode t)
(global-set-key (kbd "M-?") 'undo-tree-visualize)

(provide 'setup-undo-tree)
