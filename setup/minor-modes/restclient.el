(use-package restclient
  :straight (restclient :type git :host github :repo "MetaDark/restclient.el"
                        :upstream (:host github :repo "pashky/restclient.el"))
  :defer t)

(use-package company-restclient
  :straight t
  :after company
  :defer t
  :init
  (add-to-list 'company-backends 'company-restclient))
