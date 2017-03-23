(require-binary
 '(astyle))

(defgroup astyle nil
  "Format code using astyle."
  :group 'tools
  :group 'convenience)

(defcustom astyle-args nil
  "List of arguments to pass to astyle"
  :group 'astyle
  :type 'plist)

(defcustom astyle-mode-plist
  '(c-mode "c"
    c++-mode "c"
    java-mode "java"
    csharp-mode "cs")
  "A map of minor modes to astyle modes
Valid values: c, java, cs"
  :group 'astyle
  :type 'plist)

(defcustom astyle-reindent t
  "Whether or not to reindent the formatted code with indent-region"
  :group 'astyle
  :type 'boolean)

(defun astyle-region (start end &optional ignore-region-active)
  "Use astyle to format the selected region."
  (interactive "r")
  (let ((lang (plist-get astyle-mode-plist major-mode)))
    (if lang
        (cond ((or ignore-region-active (region-active-p))
               (apply 'call-process-region
                      (append (list start end "astyle" t t nil)
                              (list (concat "--mode=" lang))
                              astyle-args))
               (when astyle-reindent (indent-region start (point))))
              (t (message "Nothing selected")))
      (message
       "Astyle doesn't support %s. See astyle-mode-plist to map a new major-mode to an astyle language."
       (symbol-name major-mode)))))

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
        "--align-pointer=name"
        "--pad-oper"))

(global-set-key (kbd "<f7>") 'astyle-dwim)

(provide 'setup-astyle)
