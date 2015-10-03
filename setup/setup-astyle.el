(require-binary
 '(astyle))

;; Package configuration
(setq astyle-mode-alist
      '((c-mode "c")
        (c++-mode "c")
        (java-mode "java")
        (csharp-mode "cs")))

(defun astyle-region (start end &optional ignore-region-active)
  "Use astyle to format the selected region."
  (interactive "r")
  (let ((lang (cadr (assoc major-mode astyle-mode-alist))))
    (if lang
        (if (or ignore-region-active (region-active-p))
            (shell-command-on-region start end
                                     (concat "astyle "
                                             (mapconcat 'identity astyle-args " ")
                                             (concat " --mode=" lang))
                                     t t (get-buffer-create "*Astyle Errors*") t)
          (message "Nothing selected"))
      (message "Astyle doesn't support %s. See astyle-mode-alist to map a new major-mode to an astyle language." (symbol-name major-mode)))))

(defun astyle-buffer ()
  "Use astyle to format the current buffer."
  (interactive)
  (astyle-region (point-min) (point-max) t))

(defun astyle-dwim ()
  "Use astyle to format the selected region. If no region is selected, format the buffer"
  (interactive)
  (if (region-active-p)
      (astyle-region (region-beginning) (region-end))
    (astyle-buffer)))

;; User configuration
(setq astyle-args
      '("--style=attach"
        "--indent=spaces=2"
        "--align-pointer=middle"
        "--pad-oper"
        "--break-blocks=all"))

(global-set-key (kbd "<f7>") 'astyle-dwim)

(provide 'setup-astyle)
