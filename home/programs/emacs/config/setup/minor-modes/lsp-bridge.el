(use-package lsp-bridge
  :hook (lsp-bridge-mode . lsp-bridge-semantic-tokens-mode)
  :bind (:map lsp-bridge-mode-map
              ("C-M-/" . lsp-bridge-find-references)
              ("M-?" . lsp-bridge-popup-documentation))
  :init
  (require 'lsp-bridge)
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(csharp-ts-mode . lsp-bridge-csharp-lsp-server))
  (add-to-list 'lsp-bridge-default-mode-hooks 'csharp-ts-mode-hook)
  (add-to-list 'lsp-bridge-formatting-indent-alist '(csharp-ts-mode . csharp-ts-mode-indent-offset))

  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(dockerfile-ts-mode . "docker-langserver"))
  (add-to-list 'lsp-bridge-default-mode-hooks 'dockerfile-ts-mode-hook)

  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(nix-ts-mode . lsp-bridge-nix-lsp-server))
  (add-to-list 'lsp-bridge-default-mode-hooks 'nix-ts-mode-hook)
  (add-to-list 'lsp-bridge-formatting-indent-alist '(nix-ts-mode . nix-ts-mode-indent-offset))

  (global-lsp-bridge-mode)

  (with-eval-after-load 'evil-vars
    (defun evil-goto-definition-lsp-bridge (_string position)
      (setq-local lsp-bridge-jump-to-def-in-other-window nil)
      (lsp-bridge-call-file-api "find_define" (lsp-bridge--point-position position)))

    (add-to-list 'evil-goto-definition-functions #'evil-goto-definition-lsp-bridge))

  (with-eval-after-load 'evil-collection-unimpaired
    (evil-define-motion evil-collection-unimpaired-next-error (count)
      "Go to next error."
      :jump t
      (setq count (or count 1))
      (cond
       ((and (bound-and-true-p lsp-bridge-mode)
             (fboundp 'lsp-bridge-diagnostic-jump-next)
             (fboundp 'lsp-bridge-diagnostic-jump-prev))
        (cond
         ((> count 0) (dotimes (_ count) (lsp-bridge-diagnostic-jump-next)))
         ((< count 0) (dotimes (_ (- count)) (lsp-bridge-diagnostic-jump-prev)))))
       ((and (bound-and-true-p flycheck-mode)
             (fboundp 'flycheck-next-error))
        (flycheck-next-error count))
       ((and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-goto-next-error))
        (flymake-goto-next-error count))
       (:default
        (message "No linting modes are on.")))))

  :config
  (defun lsp-bridge-get-project-path-by-filepath (filename)
    (if-let ((project (project-current filename)))
     (project-root project)))

  (setq lsp-bridge-get-project-path-by-filepath #'lsp-bridge-get-project-path-by-filepath)
  (setq lsp-bridge-user-langserver-dir (concat user-emacs-config-directory "langserver"))
  (setq lsp-bridge-user-multiserver-dir (concat user-emacs-config-directory "multiserver"))
  (setq lsp-bridge-nix-lsp-server "nixd")
  (setq lsp-bridge-python-lsp-server "pylsp"))

(unless (display-graphic-p)
  (use-package acm-terminal
    :after acm
    :init
    (require 'acm-terminal)))
