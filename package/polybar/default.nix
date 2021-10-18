{ pkgs, ... }:

{
  # TODO: Run as service
  home.packages = with pkgs; [
    polybarFull
  ];

  # TODO: Generate configuration from Nix
  xdg.configFile.polybar.source = ./config;
}
