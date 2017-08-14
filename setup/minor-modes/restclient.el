(straight-use-package 'restclient)
(straight-use-package 'company-restclient)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-restclient))
