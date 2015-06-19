(add-hook 'php-mode-hook
          (lambda ()
            (flycheck-mode t)))

(provide 'language-php)
