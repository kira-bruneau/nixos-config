;; Use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Delete selection when text is inserted
(use-package delsel
  :demand t
  :config
  (delete-selection-mode t))

;; Free up shift key for custom bindings
(use-package simple
  :demand t
  :config
  (setq shift-select-mode t))
