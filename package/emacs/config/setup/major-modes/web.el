(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.vue\\'"
         "\\.jsx\\'"
         "\\.tsx\\'"
         "\\.ejs\\'")
  :bind (:map web-mode-map
              ("C-c C-o" . browse-url-of-file))
  :init
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'web-mode))

  (with-eval-after-load 'restclient
    (add-to-list 'restclient-content-type-modes '("text/html" . web-mode)))

  :config
  (setq web-mode-enable-current-element-highlight t))
