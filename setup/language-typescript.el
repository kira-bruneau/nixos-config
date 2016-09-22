(require-package
 '(tide
   typescript-mode))

(add-hook 'typescript-mode-hook #'tide-setup)

(provide 'language-typescript)
