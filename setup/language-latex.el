(require-package
 '(auctex
   latex-preview-pane))

(require-binary
 '(mupdf-tools))

(setq doc-view-pdf->png-converter-function 'doc-view-pdf->png-converter-mutool)
(setq doc-view-scale-internally nil)

(defun doc-view-pdf->png-converter-mutool (pdf png page callback)
  (doc-view-start-process
   "pdf->png" "mutool"
   `("draw"
     ,(concat "-o" png)
     ,(format "-r%d" (round doc-view-resolution))
     ,pdf
     ,@(if page `(,(format "%d" page))))
   callback))

(provide 'language-latex)
