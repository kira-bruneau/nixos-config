;; Use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Delete selection when text is inserted
(use-package delsel
  :init
  (delete-selection-mode t))

;; Free up shift key for custom navigation bindings
(use-package simple
  :config
  (setq shift-select-mode nil))

;; Allow narrowing to region
(put 'narrow-to-region 'disabled nil)

;; Define keybinding for sorting lines
(global-set-key (kbd "C-c C-s") 'sort-lines)

;; Use case insensitive sorting by default
(setq sort-fold-case t)
