(use-package dired
  :bind (:map dired-mode-map
              ("I" . image-dired-default-directory))
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alh")

  (defun image-dired-default-directory ()
    (interactive)
    (image-dired default-directory)))

(use-package image-dired
  :config
  (setq image-dired-thumbnail-storage 'standard-large))
