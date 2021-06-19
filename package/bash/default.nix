{ ... }:

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

    shellAliases = {
      # Convenience aliases for common nixos commands
      nbuild = "sudo nixos-rebuild switch && home-manager switch";
      nup = "sudo nixos-rebuild --upgrade switch && nix-channel --update && home-manager switch";
      ntest = "sudo nixos-rebuild test && home-manager switch";
      br = "broot";
    };
  };
}
