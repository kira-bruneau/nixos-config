{ ... }:

{
  programs.tmux.enable = true;

  # TODO: Generate configuration from Nix
  xdg.configFile.tmux.source = ./config;
}
