{ pkgs, ... }:

{
  programs.bash = {
    enable = true;

    historySize = -1;
    historyFileSize = -1;
    historyControl = [ "ignoredups" ];

    initExtra = ''
      # Immediately append commands to history
      trap 'history -a' DEBUG

      # Read unread history at every prompt
      export PROMPT_COMMAND='history -n''\'''${PROMPT_COMMAND:+';'}$PROMPT_COMMAND

      # Fix forward history searching
      stty -ixon
    '';
  };

  home.packages = with pkgs; [
    bash-completion
    nix-bash-completions
  ];
}
