(use-package ivy
  :straight t
  :init
  (ivy-mode)

  :config
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :straight t
  :bind (:map counsel-describe-map
              ("M-." . counsel-find-symbol))
  :init
  (counsel-mode))

;; Used to sort ivy fuzzy search results
(use-package flx
  :straight t)

;; Used to prioritize commonly used counsel-M-x commands
(use-package amx
  :straight t)
