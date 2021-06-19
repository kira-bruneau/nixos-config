{ pkgs, ... }:

{
  programs.git.enable = true;

  home.packages = with pkgs; with nur.repos.kira-bruneau; [
    gitAndTools.git-bug
  ];

  # TODO: Generate configuration from Nix
  xdg.configFile.git.source = ./config;
}
