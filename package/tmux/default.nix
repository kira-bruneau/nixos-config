{ lib, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    newSession = true;
    disableConfirmationPrompt = true;
    clock24 = true;
  };

  # Automatically start tmux on SSH sessions
  programs.bash.profileExtra = lib.mkAfter ''
    if [ -z "$TMUX" ] && [ -n "$SSH_TTY" ]; then
      exec ${pkgs.tmux}/bin/tmux
    fi
  '';
}
