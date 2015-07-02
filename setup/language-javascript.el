(package-require
 '(company-tern
   js2-mode
   json-mode ;; I prefer this over js2-mode for json files
   tern))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(setq js2-mode-show-strict-warnings nil)


;; tern
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-tern))

(add-to-list 'auto-mode-alist '("\\.tern-project$" . json-mode))

(provide 'language-javascript)
