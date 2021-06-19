{ pkgs, ... }:

{
  # TODO: Run as service
  home.packages = with pkgs; with nur.repos.kira-bruneau; [
    polybarFull
  ];

  # TODO: Generate configuration from Nix
  xdg.configFile.polybar.source = ./config;
}
