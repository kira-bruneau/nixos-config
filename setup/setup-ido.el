(require-package
 '(flx
   flx-ido
   ido-ubiquitous
   ido-vertical-mode
   smex))

;; Fuzzy find everywhere with ido
(ido-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-use-virtual-buffers t)

;; Smarter flex matching for ido
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(setq flx-ido-use-faces t)

;; Ido for M-x commands
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(provide 'setup-ido)
