{ pkgs, ... }:

{
  programs.git.enable = true;

  home.packages = with pkgs; [
    gitAndTools.git-bug
  ];

  # TODO: Generate configuration from Nix
  xdg.configFile.git.source = ./config;
}
