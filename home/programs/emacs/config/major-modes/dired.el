(use-package dired
  :bind (:map dired-mode-map
              ("I" . image-dired-default-directory))
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alh")

  (defun image-dired-default-directory ()
    (interactive)
    (image-dired default-directory)))

(use-package image-dired
  :config
  (setq image-dired-thumbnail-storage 'standard-large))

(use-package async
  :hook (dired-mode . dired-async-mode))
