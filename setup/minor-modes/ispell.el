(cond
 ((eq system-type 'windows-nt)
  (use-package ispell
    :ensure-system-package (hunspell . hunspell.portable)))
 (t
  (use-package ispell
    :ensure-system-package aspell

    ;; Arch Linux Dictionaries
    :ensure-system-package (aspell . aspell-en)

    ;; NixOS Dictionaries
    :ensure-system-package (aspell . aspell-dict-en)
    :ensure-system-package (aspell . aspell-dict-en-computers)
    :ensure-system-package (aspell . aspell-dict-en-science)

    :config
    (setq ispell-program-name "aspell"))))

(use-package ispell
  :bind (("<f8>" . ispell-word)))
