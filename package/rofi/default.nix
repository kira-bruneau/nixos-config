{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
  };

  home.packages = with pkgs; [
    (rofimoji.override {
      rofi = rofi-wayland;
    })
  ];

  # TODO: Generate configuration from Nix
  xdg.configFile.rofi.source = ./config;
}
