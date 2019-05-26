(use-package web-mode
  :straight t
  :ensure-system-package
  ((npm . npm)
   (npm . nodejs)
   (intelephense . "npm i -g intelephense"))
  :mode ("\\.html?\\'"
         "\\.vue\\'")
  :hook (web-mode . lsp)
  :bind (:map web-mode-map
              ("C-c C-o" . browse-url-of-file))
  :init
  (with-eval-after-load 'restclient
    (add-to-list 'restclient-content-type-modes '("text/html" . web-mode)))

  :config
  (setq web-mode-enable-current-element-highlight t))

(use-package web-beautify
  :straight t
  :init
  (with-eval-after-load 'js2-mode
    (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))

  (with-eval-after-load 'json-mode
    (define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

  (with-eval-after-load 'sgml-mode
    (define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

  (with-eval-after-load 'web-mode
    (define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

  (with-eval-after-load 'css-mode
    (define-key css-mode-map (kbd "C-c b") 'web-beautify-css)))
