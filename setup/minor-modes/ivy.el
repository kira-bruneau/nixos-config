;; Used to sort ivy fuzzy search results
(use-package flx
  :straight t)

(use-package ivy
  :straight t
  :init
  (require 'flx)
  (ivy-mode)

  :config
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t))
