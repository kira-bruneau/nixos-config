(cond
 ((eq system-type 'windows-nt)
  (use-package ispell
    :ensure-system-package (hunspell . hunspell.portable)))
 (t
  (use-package ispell
    :ensure-system-package aspell
    :config
    (setq ispell-program-name "aspell"))))

(use-package ispell
  :bind (("<f8>" . ispell-word)))
