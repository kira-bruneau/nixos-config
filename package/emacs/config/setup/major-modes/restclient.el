(use-package restclient
  :straight t)

(use-package company-restclient
  :straight t
  :after company
  :init
  (add-to-list 'company-backends 'company-restclient))
