(use-package dired
  :bind (:map dired-mode-map
              ("I" . image-dired-default-directory))
  :hook ((dired-mode . dired-omit-mode)
         (dired-mode . dired-hide-details-mode))
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-dwim-target t)

  (defun image-dired-default-directory ()
    (interactive)
    (image-dired default-directory)))

(use-package all-the-icons-dired
  :hook ((dired-mode . all-the-icons-dired-mode))
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package image-dired
  :config
  (setq image-dired-thumbnail-storage 'standard-large))

(use-package async
  :hook (dired-mode . dired-async-mode))
