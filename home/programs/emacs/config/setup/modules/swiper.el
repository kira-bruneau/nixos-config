(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . nil)
         ("C-S-s" . isearch-forward)
         ("C-S-r" . isearch-backward))

  :config
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-plus))))
