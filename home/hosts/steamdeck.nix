{ lib, ... }:

{
  imports = [
    ../environments/bluetooth.nix
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
  ];

  home.stateVersion = "23.11";

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
    "clonehero"
    "clonehero-unwrapped"
    "data.zip"
    "discord"
    "sm64ex"
    "steam"
    "steam-original"
    "steam-run"
    "unrar"
    "vvvvvv"
  ];
}
