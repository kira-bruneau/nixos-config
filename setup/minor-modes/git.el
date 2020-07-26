(use-package magit
  :straight t
  :ensure-system-package git
  :bind (("<f10>" . magit-file-dispatch))
  :init
  (setq magit-save-repository-buffers nil)
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  (setq transient-enable-popup-navigation t)

  (setq
   magit-repolist-columns
   '(("Name" 15 magit-repolist-column-ident nil)
     ("B<U" 3 magit-repolist-column-unpulled-from-upstream
      ((:right-align t)
       (:help-echo "Upstream changes not in branch")))
     ("B>U" 3 magit-repolist-column-unpushed-to-upstream
      ((:right-align t)
       (:help-echo "Local changes not in upstream")))
     ("Flag" 3 magit-repolist-column-flag
      ((:right-align t)))
     ("Version" 35 magit-repolist-column-version nil)
     ("Path" 99 magit-repolist-column-path nil)))

  (setq
   magit-repository-directories
   '(("/etc/nixos" . 0)
     ("~/.dotfiles" . 0)
     ("~/.dotfiles/emacs/.emacs.d" . 0)
     ("~/.dotfiles/i3" . 0)
     ("~/.dotfiles/polybar/.config/polybar" . 0)
     ("~/.dotfiles/sway/.config/sway" . 0)
     ("~/.dotfiles/waybar/.config/waybar" . 0)
     ("~/Repos" . 5)
     ("~/Dev" . 5)
     ("~/dev" . 5))))

(use-package forge
  :after magit
  :straight t
  :ensure-system-package gcc
  :init
  (require 'forge))

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
