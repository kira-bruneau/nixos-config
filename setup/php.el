(straight-use-package 'php-mode)
(straight-use-package 'web-mode)

(add-hook 'php-mode-hook
          (lambda ()
            (flycheck-mode t)))
