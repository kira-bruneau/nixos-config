(use-package dired
  :bind (:map dired-mode-map
              ("I" . image-dired-default-directory))
  :hook ((dired-mode . dired-hide-details-mode))

  :custom
  (dired-listing-switches "-Alh")
  (dired-hide-details-hide-symlink-targets nil)
  (dired-dwim-target t)

  :config
  (defun image-dired-default-directory ()
    (interactive)
    (image-dired default-directory)))

(use-package all-the-icons-dired
  :hook ((dired-mode . all-the-icons-dired-mode))
  :custom
  (all-the-icons-dired-monochrome nil))

(use-package image-dired
  :custom
  (image-dired-thumbnail-storage 'standard-large))

(use-package async
  :hook (dired-mode . dired-async-mode))
