(package-require
 '(company-tern
   js2-mode
   tern))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; tern
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-tern))

(provide 'language-javascript)
