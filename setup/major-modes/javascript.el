(straight-use-package 'company-tern)
(straight-use-package 'js2-mode)
(straight-use-package 'json-mode) ;; I prefer this over js2-mode for json files
(straight-use-package 'tern)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(setq js2-mode-show-strict-warnings nil)

;; tern
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(diminish 'tern-mode " ◎")

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-tern))

(add-to-list 'auto-mode-alist '("\\.tern-project$" . json-mode))

(defun tern-setup-project-browser ()
  "Setup a new browser tern project"
  (interactive)
  (let ((source (concat dir/conf "browser.tern-project"))
        (dest (concat default-directory ".tern-project")))
    (copy-file source dest)
    (find-file dest)))

(defun tern-setup-project-node ()
  "Setup a new node.js tern project"
  (interactive)
  (let ((source (concat dir/conf "node.tern-project"))
        (dest (concat default-directory ".tern-project")))
    (copy-file source dest)
    (find-file dest)))
