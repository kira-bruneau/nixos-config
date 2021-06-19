{ ... }:

{
  programs.waybar.enable = true;

  # TODO: Generate configuration from Nix
  xdg.configFile.waybar.source = ./config;
}
