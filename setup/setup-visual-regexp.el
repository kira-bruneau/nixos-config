(require-package
 '(visual-regexp
   visual-regexp-steroids))

(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; (define-key esc-map (kbd "M-r") 'vr/isearch-backward) ;; C-M-r
;; (define-key esc-map (kbd "M-s") 'vr/isearch-forward) ;; C-M-s

(provide 'setup-visual-regexp)
