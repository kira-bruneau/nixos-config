(use-package lsp-bridge
  :hook ((after-init . global-lsp-bridge-mode)
         (lsp-bridge-mode . lsp-bridge-semantic-tokens-mode))
  :bind (:map lsp-bridge-mode-map
              ("C-M-/" . lsp-bridge-find-references)
              ("M-?" . lsp-bridge-popup-documentation))
  :init
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
        (expand-file-name (project-root project))))

  (setq lsp-bridge-get-project-path-by-filepath #'lsp-bridge-get-project-path-by-filepath)
  (setq lsp-bridge-user-langserver-dir (concat user-emacs-config-directory "langserver"))
  (setq lsp-bridge-user-multiserver-dir (concat user-emacs-config-directory "multiserver")))

(unless (display-graphic-p)
  (use-package acm-terminal))
