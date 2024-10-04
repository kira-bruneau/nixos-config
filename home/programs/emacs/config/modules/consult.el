(use-package consult
  :commands (consult-find-file-with-preview)

  :init
  (setq read-file-name-function #'consult-find-file-with-preview)

  :bind*
  (;; Search
   ([remap isearch-forward] . consult-line)
   ([remap isearch-backward] . nil)

   ;; Yank history
   ("M-y" . consult-yank-pop)

   ;; Navigation
   ([remap goto-line] . consult-goto-line)

   ;; Buffer switching
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
   ([remap project-switch-to-buffer] . consult-project-buffer)

   :map minibuffer-local-map
   ("C-r" . consult-history)

   :map vertico-map
   ("C-M-n" . vertico-next)
   ("C-M-p" . vertico-previous))

  :custom
  (consult-preview-key '("C-M-n" "C-M-p"))
  (consult-narrow-key "<")
  (consult-async-input-throttle 0.01)
  (consult-async-input-debounce 0.01)
  (consult-async-min-input 0)
  (consult-async-split-style nil)
  (consult-preview-partial-size large-file-warning-threshold)

  :config
  (setopt consult-ripgrep-args (concat consult-ripgrep-args " --hidden --glob !.git --follow --no-messages"))
  (setopt consult-fd-args (append consult-fd-args '("--hidden" "--exclude" ".git" "--follow")))

  (defun consult-find-file-with-preview (prompt &optional dir default-filename mustmatch initial predicate)
    (interactive)
    (let* ((dir (abbreviate-file-name (expand-file-name (or dir default-directory "~/"))))
           (default-filename
            (cond
              (default-filename default-filename)
              ((null initial) buffer-file-name)
              ;; Special-case "" because (expand-file-name "" "/tmp/") returns
              ;; "/tmp" rather than "/tmp/" (bug#39057).
              ((equal "" initial) dir)
              (t (expand-file-name initial dir))))
           (default
            (when default-filename
              (if (consp default-filename)
                  (mapcar 'abbreviate-file-name default-filename)
                (abbreviate-file-name default-filename))))
           (initial
            (cond
             ((and insert-default-directory (stringp dir))
              (if initial
                  (cons (minibuffer-maybe-quote-filename (concat dir initial))
                        (length (minibuffer-maybe-quote-filename dir)))
                (minibuffer-maybe-quote-filename dir)))
             (initial (cons (minibuffer-maybe-quote-filename initial) 0))))
           (default-directory dir)
           (minibuffer-completing-file-name t))
      (consult--read #'read-file-name-internal
                     :state (consult--file-preview)
                     :prompt prompt
                     :default default
                     :initial initial
                     :require-match mustmatch
                     :predicate predicate)))

  (consult-customize
   consult-line :preview-key 'any))
