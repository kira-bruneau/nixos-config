(require-package
 '(elisp-slime-nav
   slime))

;; Eval and replace any lisp expresison
;; Source: https://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)

;; Emacs lisp
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; Shorter key binding for documentation
;; (define-key elisp-slime-nav-mode-map (kbd "C-c C-d")
;;   'elisp-slime-nav-describe-elisp-hing-at-point)

(provide 'language-lisp)
