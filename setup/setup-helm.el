(require-package
 '(helm
   helm-dash))

(require 'helm-config)

(global-set-key (kbd "s-.") 'helm-dash-at-point)

(provide 'setup-helm)
