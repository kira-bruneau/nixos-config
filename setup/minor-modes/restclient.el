(straight-use-package 'restclient)
(straight-use-package 'company-restclient)

(with-eval-after-load 'restclient
  (add-to-list 'company-backends 'company-restclient))
