{ ... }:

{
  programs.alacritty.enable = true;

  # TODO: Generate configuration from Nix
  xdg.configFile.alacritty.source = ./config;
}
