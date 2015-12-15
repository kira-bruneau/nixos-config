;; Best implementation so far: http://www.emacswiki.org/emacs/sudo-save.el

(defun sudo-edit ()
  "Re-open the current buffer with elevated privileges"
  (interactive)
  (let ((old-buffer (current-buffer)))
    (find-file (concat "/sudo::" (buffer-file-name)))
    (kill-buffer old-buffer)))

;; Try to open unwriteable files with sudo
;; TODO I should check the user ids
;; (eg. This file has been made unwritable by root, edit with sudo?)
;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (unless (file-writable-p (buffer-file-name))
;;               (when (y-or-n-p "This file is unwriteable, open with sudo?")
;;                 (sudo-edit)))))

;; TODO Try to open unreadable files with sudo

;; TODO Try to open unreadable directories with sudo

(provide 'setup-sudo-edit)
