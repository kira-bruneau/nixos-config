;; Best implementation so far: http://www.emacswiki.org/emacs/sudo-save.el

(defun sudo-edit ()
  "Re-open a file with elevated privileges"
  (interactive)
  (cond
   ((not (file-writable-p buffer-file-name))
    (write-file (format "/sudo::%s" buffer-file-name)))
   (t
    (message "current buffer is already writeable"))))

(add-hook 'find-file-hook
          (lambda ()
            (unless (file-writable-p (buffer-file-name))
              (when (y-or-n-p "This file is unwriteable, open with sudo?")
                (print (concat "/sudo::" buffer-file-name))))))

(provide 'setup-sudo-edit)
