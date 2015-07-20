(require-package
 '(diminish))

;; (eval-after-load "projectile"
;;   '(diminish 'mode-projectile ""))

(eval-after-load "company"
  '(diminish 'company-mode " ▶"))

(eval-after-load "flycheck"
  '(diminish 'flycheck-mode " ✓"))

(eval-after-load "elisp-slime-nav"
  '(diminish 'elisp-slime-nav-mode " ◎"))

(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode " ψ"))

(eval-after-load "git-gutter"
  '(diminish 'git-gutter-mode ""))

(eval-after-load "drag-stuff"
  '(diminish 'drag-stuff-mode " ↕"))

(eval-after-load "smartparens"
  '(diminish 'smartparens-mode " ()"))

(eval-after-load "flyspell"
  '(diminish 'flyspell-mode " ≈"))

(eval-after-load "projectile"
  '(diminish 'projectile-mode ""))

(diminish 'whitespace-cleanup-mode " ⌫") ;; ⌧
(diminish 'visual-line-mode " ↩")

(diminish 'subword-mode "w̲")

(provide 'setup-diminish)
