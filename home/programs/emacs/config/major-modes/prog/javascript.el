(use-package js
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :interpreter (("node" . js-ts-mode)
                ("zx" . js-ts-mode))
  :init
  (add-to-list 'compilation-error-regexp-alist 'node-stack)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(node-stack "[ \t]+at \\(.* (\\(/.+?\\):\\([0-9]+\\):\\([0-9]+\\))\\)" 2 3 4 2 1))

  (add-to-list 'compilation-error-regexp-alist 'node-stack-unamed)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(node-stack-unamed "[ \t]+at \\(\\(/.+?\\):\\([0-9]+\\):\\([0-9]+\\)\\)" 2 3 4 2 1))

  ;; Java compilation error regexp conflicts with node-stack
  (add-to-list 'compilation-error-regexp-alist-alist
               '(java nil))

  ;; Ada compilation error regexp conflicts with node-stack-unamed
  (add-to-list 'compilation-error-regexp-alist-alist
               '(ada nil))

  (with-eval-after-load 'flycheck
    (setopt flycheck-javascript-eslint-executable "eslint_d"))

  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-formatters '(eslint . ("eslint_d" "--stdin" "--fix-to-stdout" "--stdin-filename" filepath))))

  (with-eval-after-load 'restclient
    (add-to-list 'restclient-content-type-modes '("application/js" . js-ts-mode))))

(use-package typescript-ts-mode
  :mode ("\\.ts\\'"
         ("\\.tsx\\'" . tsx-ts-mode)))
