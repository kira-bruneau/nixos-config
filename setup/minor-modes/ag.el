(straight-use-package 'ag)
;; (straight-use-package 'wgrep-ag)

(add-hook 'ag-mode-hook
          (lambda ()
            (next-error-follow-minor-mode 1)))

(add-hook 'ag-search-finished-hook
          (lambda ()
            (other-window 1)))
