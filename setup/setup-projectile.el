(projectile-global-mode)
(setq projectile-indexing-method 'alien)

(add-hook 'ag-mode-hook
          (lambda ()
            (next-error-follow-minor-mode 1)))

(defun file-name-references ()
  (interactive)
  (projectile-ag (file-name-sans-extension (buffer-name))))

(global-set-key (kbd "C-c C-.") 'file-name-references)

(provide 'setup-projectile)
