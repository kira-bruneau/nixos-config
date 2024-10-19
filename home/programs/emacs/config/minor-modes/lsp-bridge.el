(use-package lsp-bridge
  :hook ((after-init . global-lsp-bridge-mode)
         (lsp-bridge-mode . lsp-bridge-semantic-tokens-mode))
  :bind (:map
         lsp-bridge-mode-map
         ("C-M-/" . lsp-bridge-find-references)
         ("M-?" . lsp-bridge-popup-documentation)

         :map lsp-bridge-ref-mode-map
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
         ("X" . nil)

         :map project-prefix-map
         ("l" . lsp-bridge-restart-process))
  :init
  (with-eval-after-load 'evil-core
    (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)
    (evil-set-initial-state 'lsp-bridge-call-hierarchy-mode-map 'emacs))

  (with-eval-after-load 'evil-vars
    (defun evil-goto-definition-lsp-bridge (_string position)
      (setq-local lsp-bridge-jump-to-def-in-other-window nil)
      (lsp-bridge-call-file-api "find_define" (lsp-bridge--point-position position)))

    (add-to-list 'evil-goto-definition-functions #'evil-goto-definition-lsp-bridge))

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

  :config
  (defun lsp-bridge-get-project-path-by-filepath (filename)
    (if-let ((project (project-current filename)))
        (expand-file-name (project-root project))))

  (defadvice lsp-bridge-ref-switch-to-edit-mode (after lsp-bridge-ref-switch-to-view-mode activate)
    (evil-exit-emacs-state))

  (defadvice lsp-bridge-ref-switch-to-view-mode (after lsp-bridge-ref-switch-to-view-mode activate)
    (evil-emacs-state)))

(unless (display-graphic-p)
  (use-package acm-terminal :demand))
