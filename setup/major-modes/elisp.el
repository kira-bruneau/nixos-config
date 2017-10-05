(straight-use-package 'macrostep)

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'macrostep-expand)

;; Eval and replace any lisp expresison
;; Source: https://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))
