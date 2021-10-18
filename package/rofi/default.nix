{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
  };

  # TODO: Generate configuration from Nix
  xdg.configFile.rofi.source = ./config;
}
