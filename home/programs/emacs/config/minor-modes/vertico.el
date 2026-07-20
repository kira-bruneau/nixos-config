(use-package vertico :demand
  :custom
  (vertico-cycle t)

  :config
  (vertico-mode))

(use-package vertico-directory
  :bind (:map vertico-map
         ("/" . vertico-directory-enter)
         ("~" . vertico-directory-home)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (defun vertico-directory-enter (&optional arg c)
    (interactive "P")
    (if (and
         (not arg)
         (eq (vertico--metadata-get 'category) 'file)
         (not (string-equal vertico--base ""))
         (>= vertico--index 0)
         (eq (point) (point-max)))
        (let ((cand (vertico--candidate)))
          (if (file-regular-p cand)
              (vertico-exit)
            (progn
              (delete-minibuffer-contents)
              (insert cand))))
      (self-insert-command (prefix-numeric-value arg) c)))

  (defun vertico-directory-home ()
    (interactive)
    (insert "~/")))
