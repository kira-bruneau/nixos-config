(straight-use-package 'ggtags)

(pacaur-use-packages
 '(global
   ctags))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'php-mode)
              (ggtags-mode 1))))

(diminish 'ggtags-mode " ◎")

(global-set-key (kbd "M-,") 'pop-tag-mark)
