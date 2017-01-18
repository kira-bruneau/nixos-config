(require-package
 '(arduino-mode))

(require-binary
 '(arduino-mk))

(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

(provide 'language-arduino)
