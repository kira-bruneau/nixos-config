{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    package = pkgs.nur.repos.kira-bruneau.rofi-wayland;
  };

  # TODO: Generate configuration from Nix
  xdg.configFile.rofi.source = ./config;
}
