(use-package restclient
  :straight (restclient :type git :host github :repo "MetaDark/restclient.el"
                        :upstream (:host github :repo "pashky/restclient.el"))
  :commands (restclient-mode))

(use-package company-restclient
  :straight t
  :commands (company-restclient)
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-restclient)))
