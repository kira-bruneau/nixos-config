(use-package whitespace
  :hook diff-mode
  :custom
  (whitespace-style
   '(face
     tabs spaces trailing lines-tail space-before-tab
     newline indentation empty space-after-tab tab-mark)))
