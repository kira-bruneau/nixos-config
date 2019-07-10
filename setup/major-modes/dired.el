(use-package dired
  :bind (:map dired-mode-map
              ("E" . xah-open-in-external-app))
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-Alh")

  (defun xah-open-in-external-app ()
    "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2016-10-15"
    (interactive)
    (let* (
           (-file-list
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name))))
           (-do-it-p (if (<= (length -file-list) 5)
                         t
                       (y-or-n-p "Open more than 5 files? "))))
      (when -do-it-p
        (cond
         ((string-equal system-type "windows-nt")
          (mapc
           (lambda (-fpath)
             (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" -fpath t t))) -file-list))
         ((string-equal system-type "darwin")
          (mapc
           (lambda (-fpath)
             (shell-command
              (concat "open " (shell-quote-argument -fpath))))  -file-list))
         ((string-equal system-type "gnu/linux")
          (mapc
           (lambda (-fpath) (let ((process-connection-type nil))
                              (start-process "" nil "xdg-open" -fpath))) -file-list)))))))
