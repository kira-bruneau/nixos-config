(projectile-global-mode)
(setq projectile-indexing-method 'alien)

(add-hook 'ag-mode-hook
          (lambda ()
            (next-error-follow-minor-mode 1)))

(provide 'setup-projectile)
