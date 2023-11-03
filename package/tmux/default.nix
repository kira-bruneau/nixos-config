{ ... }:

{
  programs.tmux = {
    enable = true;
    newSession = true;
    disableConfirmationPrompt = true;
  };
}
