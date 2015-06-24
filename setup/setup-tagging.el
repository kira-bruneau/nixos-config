(package-require
 '(ggtags))

(global-set-key (kbd "M-,") 'pop-tag-mark)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'php-mode)
              (ggtags-mode 1))))

(provide 'setup-tagging)
