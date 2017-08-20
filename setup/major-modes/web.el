(straight-use-package 'web-beautify)
(straight-use-package 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(with-eval-after-load 'js2-mode
  (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))

(with-eval-after-load 'json-mode
  (define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(with-eval-after-load 'sgml-mode
  (define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(with-eval-after-load 'web-mode
  (define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

(with-eval-after-load 'css-mode
  (define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(with-eval-after-load 'restclient
  (add-to-list 'restclient-media-type-mode-alist '("\\`text/html\\'" . web-mode)))
