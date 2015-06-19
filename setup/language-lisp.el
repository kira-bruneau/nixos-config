;; elisp
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")
;; (setq slime-contribs '(slime-fancy))
;; (require 'slime)
;; (slime-setup)

(provide 'language-lisp)
