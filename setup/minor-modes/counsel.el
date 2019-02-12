;; Used to prioritize commonly used counsel-M-x commands
(use-package amx
  :straight t)

(use-package counsel
  :straight t
  :bind (:map counsel-describe-map
              ("M-." . counsel-find-symbol))
  :init
  (require 'amx)
  (counsel-mode)

  :config
  (add-to-list 'ivy-sort-functions-alist '(counsel-fzf . nil)))
