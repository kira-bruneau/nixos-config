(use-package web-mode
  :straight t
  :mode ("\\.html?\\'"
         "\\.vue\\'"
         "\\.jsx\\'"
         "\\.tsx\\'"
         "\\.ejs\\'")
  :hook (sgml-electric-tag-pair-mode . web-mode)
  :bind (:map web-mode-map
              ("C-c C-o" . browse-url-of-file))
  :init
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'web-mode))

  (with-eval-after-load 'restclient
    (add-to-list 'restclient-content-type-modes '("text/html" . web-mode)))

  :config
  (setq web-mode-enable-current-element-highlight t))
