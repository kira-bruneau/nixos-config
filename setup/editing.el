;; Use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Delete selection when text is inserted
(use-package delsel
  :init
  (delete-selection-mode t))

;; Free up shift key for custom bindings
(use-package simple
  :config
  (setq shift-select-mode nil))
