(use-package vertico :demand
  :custom
  (vertico-cycle t)

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
    (if-let (((eq n 1))
             ((>= vertico--index 0))
             ((eq (vertico--metadata-get 'category) 'file))
             ((eq (point) (point-max)))
             ((not (eq (char-before) ?/)))
             (pwd (file-name-directory (minibuffer-contents-no-properties)))
             (cand (vertico--candidate))
             ((string-prefix-p pwd cand)))
        (progn
          (delete-minibuffer-contents)
          (insert cand))
      (self-insert-command n c)))

  (defun vertico-directory-home ()
    (interactive)
    (insert "~/")))
