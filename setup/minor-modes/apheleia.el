(use-package apheleia
  :straight t
  :config
  (add-to-list 'apheleia-formatters '(prettier . ("prettierc" filepath)))
  (add-to-list 'apheleia-formatters '(eslint . ("eslint_c" "--stdin" "--fix-to-stdout" "--stdin-filename" filepath))))
