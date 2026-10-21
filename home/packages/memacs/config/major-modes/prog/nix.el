(use-package nix-ts-mode
  :mode "\\.nix\\'")

;; Derived from https://github.com/NixOS/nix-mode/blob/719feb7868fb567ecfe5578f6119892c771ac5e5/nix-drv-mode.el
(define-derived-mode nix-drv-mode json-ts-mode "Nix-Derivation"
  "Pretty print Nix’s .drv files."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (call-process "nix" nil '(t nil) nil "derivation" "show" "--quiet" (buffer-file-name))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (read-only-mode 1))
  (add-hook 'change-major-mode-hook #'nix-drv-mode-dejsonify-buffer nil t))

(defun nix-drv-mode-dejsonify-buffer ()
  "Restore nix-drv-mode when switching to another mode."
  (remove-hook 'change-major-mode-hook #'nix-drv-mode-dejsonify-buffer t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents (buffer-file-name))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (read-only-mode nil)))

(add-to-list 'auto-mode-alist '("^/nix/store/.+\\.drv\\'" . nix-drv-mode))
