;; Eval and replace any lisp expresison
;; Source: https://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)

;; Allow ediffs in TRAMP
;; Source: http://www.emacswiki.org/emacs/EdiffMode
(defun ediff-listable-file (file-name)
  (let ((handler (find-file-name-handler file-name 'file-local-copy)))
    (or (null handler) (eq handler 'dired-handler-fn) (eq handler 'tramp-file-name-handler))))

(defun ediff-same-file-contents (f1 f2)
  "Return t if files F1 and F2 have identical contents."
  (if (and (not (file-directory-p f1))
           (not (file-directory-p f2)))
      (let ((res
             (apply 'call-process ediff-cmp-program nil nil nil
                    (append ediff-cmp-options
                            (list (if (tramp-tramp-file-p f1)
                                      (tramp-handle-file-local-copy f1)
                                    f1)
                                  (if (tramp-tramp-file-p f2)
                                      (tramp-handle-file-local-copy f2)
                                    f2))))))
        (and (numberp res) (eq res 0)))))

(defun sudo-edit ()
  "Re-open a file with elevated privileges"
  (interactive)
  (cond
   ((not (file-writable-p buffer-file-name))
    (write-file (format "/sudo::%s" buffer-file-name)))
   (t
    (message "current buffer is already writeable"))))

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

(provide 'setup-custom)
