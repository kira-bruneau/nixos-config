;; (ggtags-update-on-save t) ;; Will be available in version 0.8.10
(global-set-key (kbd "M-,") 'pop-tag-mark)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'php-mode)
              (ggtags-mode 1))))

(provide 'setup-tagging)
