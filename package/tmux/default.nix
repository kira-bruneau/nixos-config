{ ... }:

{
  programs.tmux = {
    enable = true;
    newSession = true;
    disableConfirmationPrompt = true;
    clock24 = true;
  };
}
