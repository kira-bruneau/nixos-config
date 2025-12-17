(use-package lsp-bridge
  :commands (lsp-bridge-find-def
             lsp-bridge-find-references
             lsp-bridge-rename
             lsp-bridge-restart-process
             lsp-bridge-show-documentation)

  :hook ((after-init . global-lsp-bridge-mode)
         (lsp-bridge-mode . lsp-bridge-semantic-tokens-mode))

  :bind (:map lsp-bridge-ref-mode-map
         ("n" . lsp-bridge-ref-jump-next-keyword)
         ("e" . lsp-bridge-ref-jump-prev-keyword)
         ("i" . lsp-bridge-ref-jump-prev-file)
         ("u" . lsp-bridge-ref-switch-to-edit-mode)
         ("f" . lsp-bridge-ref-filter-match-files)
         ("F" . lsp-bridge-ref-filter-mismatch-files)
         ("x" . lsp-bridge-ref-unfilter)
         ("s" . lsp-bridge-ref-filter-match-results)
         ("S" . lsp-bridge-ref-filter-mismatch-results)
         ("j" . nil)
         ("k" . nil)
         ("l" . nil)
         ("X" . nil))
  :init
  (with-eval-after-load 'evil-core
    (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)
    (evil-set-initial-state 'lsp-bridge-call-hierarchy-mode-map 'emacs))

  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'lsp-bridge-ref-mode-edit-map
      (kbd "<escape>") 'lsp-bridge-ref-switch-to-view-mode))

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

  :custom
  (lsp-bridge-get-project-path-by-filepath #'lsp-bridge-get-project-path-by-filepath)
  (lsp-bridge-user-langserver-dir (concat user-emacs-config-directory "langserver"))
  (lsp-bridge-user-multiserver-dir (concat user-emacs-config-directory "multiserver"))
  (lsp-bridge-diagnostic-fetch-idle 0)
  (lsp-bridge-code-action-enable-popup-menu nil)
  (lsp-bridge-semantic-tokens-auto-update 'hook)
  (lsp-bridge-semantic-tokens-delay 0)
  (acm-backend-lsp-show-progress t)

  :config
  ;; Don't run on lisp modes without a backing language server
  (setopt
   lsp-bridge-default-mode-hooks
   (cl-set-difference
    lsp-bridge-default-mode-hooks
    '(emacs-lisp-mode-hook lisp-interaction-mode-hook)))

  (defun lsp-bridge-get-project-path-by-filepath (filename)
    (if-let ((project (project-current filename)))
        (expand-file-name (project-root project))))

  (define-advice lsp-bridge-ref-switch-to-edit-mode (:after () exit-evil-emacs-state)
    (evil-exit-emacs-state))

  (define-advice lsp-bridge-ref-switch-to-view-mode (:after () enter-evil-emacs-state)
    (evil-emacs-state))

  (define-advice lsp-bridge-restart-process (:after () revert-buffer)
    (revert-buffer)))

(unless (display-graphic-p)
  (use-package acm-terminal :demand))
