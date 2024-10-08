;; Start server
(require 'server)
(unless (server-running-p) (server-start))

;; Use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Delete selection when text is inserted
(delete-selection-mode)

;; Free up shift key for custom navigation bindings
(use-package simple
  :custom
  (shift-select-mode nil))

;; Allow narrowing to region
(put 'narrow-to-region 'disabled nil)

;; Define keybinding for sorting lines
(global-set-key (kbd "C-c C-s") 'sort-lines)

;; Use case insensitive sorting by default
(setopt sort-fold-case t)
