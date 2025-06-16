(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.vue\\'"
         "\\.ejs\\'"
         "\\.handlebars\\'")
  :init
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'web-mode))

  (with-eval-after-load 'restclient
    (add-to-list 'restclient-content-type-modes '("text/html" . web-mode)))

  :custom
  (web-mode-enable-current-element-highlight t))
