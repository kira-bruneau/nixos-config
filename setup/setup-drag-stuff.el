(require-package
 '(drag-stuff))

(drag-stuff-global-mode t)
(diminish 'drag-stuff-mode) ;; ↕

(global-set-key (kbd "M-p") 'drag-stuff-up)
(global-set-key (kbd "M-n") 'drag-stuff-down)

(provide 'setup-drag-stuff)
