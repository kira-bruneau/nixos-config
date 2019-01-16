(use-package restclient
  :straight (restclient :type git :host github :repo "MetaDark/restclient.el"
                        :fork (:host github :repo "pashky/restclient.el")))

(use-package company-restclient
  :straight t
  :after company
  :init
  (add-to-list 'company-backends 'company-restclient))
