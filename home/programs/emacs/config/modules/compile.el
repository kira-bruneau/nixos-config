(use-package compile
  :config
  (define-advice compile (:around (fn command &optional comint) force-comint)
    "Advises `compile' to always set argument COMINT to t."
    (funcall fn command t)))
