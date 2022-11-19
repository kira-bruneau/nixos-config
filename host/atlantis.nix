{ lib, ... }:

{
  imports = [
    ../environment/cli.nix
    ../environment/gaming.nix
    ../environment/gui.nix
  ];

  programs.waybar.settings.mainBar.temperature.thermal-zone = 5;

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "bcml"
    "clonehero"
    "clonehero-unwrapped"
    "discord"
    "runescape-launcher"
    "sm64ex"
    "steam"
    "steam-original"
    "steam-run"
    "unrar"
    "VVVVVV"
    "VVVVVV-with-assets"
  ];

  home.stateVersion = "21.05";
}
