(straight-use-package 'arduino-mode)

(pacaur-use-packages
 '(arduino-mk))

(add-to-list 'auto-mode-alist '("\\.\\(pde\\|ino\\)\\'" . arduino-mode))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)
