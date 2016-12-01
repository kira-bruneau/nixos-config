(require-package
 '(tide
   typescript-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode t)
  (company-mode t)
  (eldoc-mode t)
  (tide-hl-identifier-mode t))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(provide 'language-typescript)
