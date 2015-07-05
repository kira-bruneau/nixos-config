(require-package
 '(php-mode
   web-mode))

(add-hook 'php-mode-hook
          (lambda ()
            (flycheck-mode t)))

(provide 'language-php)
