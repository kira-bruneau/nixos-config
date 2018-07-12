(use-package buffer-move
  :straight t

  ;; Mirror i3 window move commands (vim-like + colemak layout)
  :bind (("C-S-n" . buf-move-left)
         ("C-S-e" . buf-move-down)
         ("C-S-i" . buf-move-up)
         ("C-S-o" . buf-move-right)))
