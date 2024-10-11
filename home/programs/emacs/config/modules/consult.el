(use-package consult
  :commands (completing-read-with-preview)

  :init
  (setq completing-read-function #'completing-read-with-preview)

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

  (defun completing-read-with-preview (prompt collection
                                              &optional
                                              predicate require-match
                                              initial-input hist def
                                              inherit-input-method)
    (cond
     ((and
       (not (boundp 'in-consult-read-p))
       (eq
        (completion-metadata-get
         (completion-metadata initial-input collection predicate)
         'category)
        'file))
      (consult--read collection
                     :prompt prompt
                     :predicate predicate
                     :require-match require-match
                     :initial initial-input
                     :history hist
                     :default def
                     :inherit-input-method inherit-input-method
                     :state (consult--file-preview)))
     (t (completing-read-default prompt collection predicate
                                 require-match initial-input hist def
                                 inherit-input-method))))

  (defun consult-read-with-preview (oldfun table &rest options)
    (let ((in-consult-read-p t))
      (unless (plist-get options :state)
        (when (eq (plist-get options :category) 'file)
          (plist-put options :state (consult--file-preview))))
      (apply oldfun table options)))

  (add-function :around (symbol-function 'consult--read) #'consult-read-with-preview)

  (consult-customize
   consult-line :preview-key 'any))
