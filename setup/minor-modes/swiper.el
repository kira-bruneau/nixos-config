(use-package swiper
  :straight t
  :after ivy
  :bind (("C-s" . swiper))
  :config
  (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-plus)))
