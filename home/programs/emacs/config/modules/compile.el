;; todo move to compilation-mode

(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t."
  (ad-set-arg 1 t))

(defadvice comint-term-environment (after disable-term-pager activate)
  "Disable pager in term environment"
  (setq ad-return-value (cons "PAGER=" ad-return-value)))
