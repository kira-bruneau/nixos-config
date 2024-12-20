(use-package org
  :bind* (("C-c l" . org-store-link)
          ("C-c a" . org-agenda)
          :map org-mode-map
          ("M-p" . org-metaup)
          ("M-n" . org-metadown))

  :init
  (with-eval-after-load 'lsp-bridge
    (delete 'org-mode-hook lsp-bridge-default-mode-hooks))

  :custom
  (org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-entities-user
   '("* Letters"
     "** Latin (blackboard bold face)"
     ("Abb" "\\mathbb{A}" nil "&#120120;" "A" "A" "𝔸")
     ("abb" "\\mathbb{a}" nil "&#120146;" "a" "a" "𝕒")
     ("Bbb" "\\mathbb{B}" nil "&#120121;" "B" "B" "𝔹")
     ("bbb" "\\mathbb{b}" nil "&#120147;" "b" "b" "𝕓")
     ("Cbb" "\\mathbb{C}" nil "&#8450;" "C" "C" "ℂ")
     ("cbb" "\\mathbb{c}" nil "&#120148;" "c" "c" "𝕔")
     ("Dbb" "\\mathbb{D}" nil "&#120123;" "D" "D" "𝔻")
     ("dbb" "\\mathbb{d}" nil "&#120149;" "d" "d" "𝕕")
     ("Ebb" "\\mathbb{E}" nil "&#120124;" "E" "E" "𝔼")
     ("ebb" "\\mathbb{e}" nil "&#120150;" "e" "e" "𝕖")
     ("Fbb" "\\mathbb{F}" nil "&#120125;" "F" "F" "𝔽")
     ("fbb" "\\mathbb{f}" nil "&#120151;" "f" "f" "𝕗")
     ("Gbb" "\\mathbb{G}" nil "&#120126;" "G" "G" "𝔾")
     ("gbb" "\\mathbb{g}" nil "&#120152;" "g" "g" "𝕘")
     ("Hbb" "\\mathbb{H}" nil "&#8461;" "H" "H" "ℍ")
     ("hbb" "\\mathbb{h}" nil "&#120153;" "h" "h" "𝕙")
     ("Ibb" "\\mathbb{I}" nil "&#120128;" "I" "I" "𝕀")
     ("ibb" "\\mathbb{i}" nil "&#120154;" "i" "i" "𝕚")
     ("Jbb" "\\mathbb{J}" nil "&#120129;" "J" "J" "𝕁")
     ("jbb" "\\mathbb{j}" nil "&#120155;" "j" "j" "𝕛")
     ("Kbb" "\\mathbb{K}" nil "&#120130;" "K" "K" "𝕂")
     ("kbb" "\\mathbb{k}" nil "&#120156;" "k" "k" "𝕜")
     ("Lbb" "\\mathbb{L}" nil "&#120131;" "L" "L" "𝕃")
     ("lbb" "\\mathbb{l}" nil "&#120157;" "l" "l" "𝕝")
     ("Mbb" "\\mathbb{M}" nil "&#120132;" "M" "M" "𝕄")
     ("mbb" "\\mathbb{m}" nil "&#120158;" "m" "m" "𝕞")
     ("Nbb" "\\mathbb{N}" nil "&#8469;" "N" "N" "ℕ")
     ("nbb" "\\mathbb{n}" nil "&#120159;" "n" "n" "𝕟")
     ("Obb" "\\mathbb{O}" nil "&#120134;" "O" "O" "𝕆")
     ("obb" "\\mathbb{o}" nil "&#120160;" "o" "o" "𝕠")
     ("Pbb" "\\mathbb{P}" nil "&#8473;" "P" "P" "ℙ")
     ("pbb" "\\mathbb{p}" nil "&#120161;" "p" "p" "𝕡")
     ("Qbb" "\\mathbb{Q}" nil "&#8474;" "Q" "Q" "ℚ")
     ("qbb" "\\mathbb{q}" nil "&#120162;" "q" "q" "𝕢")
     ("Rbb" "\\mathbb{R}" nil "&#8477;" "R" "R" "ℝ")
     ("rbb" "\\mathbb{r}" nil "&#120163;" "r" "r" "𝕣")
     ("Sbb" "\\mathbb{S}" nil "&#120138;" "S" "S" "𝕊")
     ("sbb" "\\mathbb{s}" nil "&#120164;" "s" "s" "𝕤")
     ("Tbb" "\\mathbb{T}" nil "&#120139;" "T" "T" "𝕋")
     ("tbb" "\\mathbb{t}" nil "&#120165;" "t" "t" "𝕥")
     ("Ubb" "\\mathbb{U}" nil "&#120140;" "U" "U" "𝕌")
     ("ubb" "\\mathbb{u}" nil "&#120166;" "u" "u" "𝕦")
     ("Vbb" "\\mathbb{V}" nil "&#120141;" "V" "V" "𝕍")
     ("vbb" "\\mathbb{v}" nil "&#120167;" "v" "v" "𝕧")
     ("Wbb" "\\mathbb{W}" nil "&#120142;" "W" "W" "𝕎")
     ("wbb" "\\mathbb{w}" nil "&#120168;" "w" "w" "𝕨")
     ("Xbb" "\\mathbb{X}" nil "&#120143;" "X" "X" "𝕏")
     ("xbb" "\\mathbb{x}" nil "&#120169;" "x" "x" "𝕩")
     ("Ybb" "\\mathbb{Y}" nil "&#120144;" "Y" "Y" "𝕐")
     ("ybb" "\\mathbb{y}" nil "&#120170;" "y" "y" "𝕪")
     ("Zbb" "\\mathbb{Z}" nil "&#8484;" "Z" "Z" "ℤ")
     ("zbb" "\\mathbb{z}" nil "&#120171;" "z" "z" "𝕫")
     "** Extra math symbols"
     ("mid" "\\mid" nil "&#124;" "|" "|" "|")
     ("nmid" "\\nmid" nil "&#8740;" "[not divides]" "[not divides]" "∤")
     ("oint" "\\oint" t "&oint;" "[contour integral]" "[contour integral]" "∮")))

  (org-agenda-files (directory-files-recursively "~/Documents/Notes" "\.org$"))

  :config
  (defcustom org-interpret-symbols nil
    "Non-nil means never interpret symbols to mean something else.
When nil, certain symbols are interpreted and displayed differently."
    :group 'org-appearance
    :version "24.1"
    :type 'boolean)

  (defun org-toggle-vertatim ()
    "Toggle between verbatim text and interpreted text.
This is a hacky solution designed for only my use case"
    (interactive)
    (org-set-local 'org-interpret-symbols (not org-interpret-symbols))
    (org-set-local 'org-pretty-entities org-interpret-symbols)
    (org-set-local 'org-hide-emphasis-markers org-interpret-symbols)
    (org-restart-font-lock)))
