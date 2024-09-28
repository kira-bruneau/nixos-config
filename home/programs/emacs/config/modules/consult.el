(use-package consult
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

  :config
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden --glob !.git --follow --no-messages"))
  (setq consult-fd-args (append consult-fd-args '("--hidden" "--exclude" ".git" "--follow")))

  (consult-customize
   consult-line :preview-key 'any))
