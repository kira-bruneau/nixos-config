(use-package elisp-mode
  :demand t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . fc-eval-and-replace))
  :config
  ;; Eval and replace any lisp expresison
  ;; Source: https://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
  (defun fc-eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (prin1 (eval (read (current-kill 0)))
           (current-buffer))))

(use-package macrostep
  :straight t
  :after elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c C-e" . macrostep-expand)))
