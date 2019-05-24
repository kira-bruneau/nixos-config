(use-package swiper
  :straight t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . nil)
         ("C-S-s" . isearch-forward)
         ("C-S-r" . isearch-backward))
  :config
  (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-plus)))
