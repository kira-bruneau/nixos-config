(straight-use-package 'php-mode)

(add-hook 'php-mode-hook
          (lambda ()
            (flycheck-mode t)))
