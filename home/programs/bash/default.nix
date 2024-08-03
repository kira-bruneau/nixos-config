{ pkgs, ... }:

{
  programs.bash = {
    enable = true;

    historySize = -1;
    historyFileSize = -1;
    historyControl = [ "ignoredups" ];
    initExtra = builtins.readFile ./.bashrc;
  };

  home.packages = with pkgs; [
    bash-completion
    nix-bash-completions
  ];
}
