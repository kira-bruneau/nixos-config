(use-package vertico
  :demand
  :config
  (vertico-mode))

(use-package vertico-directory
  :bind (:map vertico-map
         ("/" . vertico-directory-insert)
         ("~" . vertico-directory-home)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (defun vertico-directory-insert (n &optional c)
    (interactive "p")
    (if (and (eq 'file (vertico--metadata-get 'category))
             (eq n 1)
             (eq (point) (point-max))
             (not (eq (char-before) ?/)))
        (vertico-insert)
      (self-insert-command n c)))

  (defun vertico-directory-home ()
    (interactive)
    (insert "~/")))
