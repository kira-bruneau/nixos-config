(use-package magit
  :straight (magit :type git :host github :repo "MetaDark/magit"
                   :fork (:host github :repo "magit/magit"))
  :ensure-system-package git
  :bind (("C-c C-b" . magit-blame)
         ("C-c C-l" . magit-log-buffer-file))
  :init
  (setq magit-save-repository-buffers nil)
  (setq magit-no-message '("Turning on magit-auto-revert-mode...")))

(use-package git-timemachine
  :straight t
  :bind ("C-c C-t" . git-timemachine))

(use-package browse-at-remote
  :straight t
  :bind ("C-c o" . browse-at-remote))

(when (eq system-type 'windows-nt)
  (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/bin"))
  (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/usr/bin"))
  (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH")))
  (setenv "PATH" (concat "C:\\Program Files\\Git\\usr\\bin;" (getenv "PATH")))
  (setenv "SSH_ASKPASS" "git-gui--askpass")
  (use-package ssh-agency
    :straight t))
