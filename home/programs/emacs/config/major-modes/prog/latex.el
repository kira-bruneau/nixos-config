(use-package tex-mode
  :bind (:map latex-mode-map
              ("M-p" . latex-preview-pane-mode)))

(use-package latex-preview-pane
  :config
  ;; Fix lpp/window-containing-preview
  ;; https://github.com/jsinglet/latex-preview-pane/pull/58
  (defun lpp/window-containing-preview ()
    (let (windows i docViewWindow)
      (let ((windows (cl-reduce #'append (mapcar `window-list (frame-list)))))
        (setq windows (cl-reduce #'append (mapcar `window-list (frame-list))))
        (cl-find-if (lambda (window) (window-parameter window 'is-latex-preview-pane)) windows)))))
