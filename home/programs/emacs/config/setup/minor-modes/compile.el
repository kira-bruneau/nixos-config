(use-package compile
  :config
  (defadvice compile (before ad-compile-smart activate)
    "Advises `compile' so it sets the argument COMINT to t."
    (ad-set-arg 1 t)))
