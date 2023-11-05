{ lib, pkgs, ... }:

let
  tmux = pkgs.tmux;
in
{
  programs.tmux = {
    enable = true;
    package = tmux;
    newSession = true;
    disableConfirmationPrompt = true;
    clock24 = true;
  };

  # Automatically start tmux on SSH sessions
  programs.bash.profileExtra = lib.mkAfter ''
    if [ -z "$TMUX" ] && [ -n "$SSH_TTY" ]; then
      exec ${tmux}/bin/tmux
    fi
  '';
}
