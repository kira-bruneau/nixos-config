{ lib, ... }:

{
  imports = [
    ../environment/cli.nix
    ../environment/gaming.nix
    ../environment/gui.nix
  ];

  services.syncthing.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "bcml"
    "clonehero"
    "clonehero-unwrapped"
    "discord"
    "runescape-launcher"
    "slack"
    "sm64ex"
    "steam"
    "steam-original"
    "steam-runtime"
    "unrar"
    "VVVVVV"
    "VVVVVV-with-assets"
  ];

  home.stateVersion = "21.05";
}
