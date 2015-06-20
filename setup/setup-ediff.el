;; Restore window configuration after quitting ediff
;; Source: http://www.emacswiki.org/emacs/EdiffMode

;; Some custom configuration to ediff
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
                  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
                  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)

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

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(provide 'setup-ediff)
