{ lib, ... }:

{
  imports = [
    ../environments/gui/sway.nix
    ../users/kira.nix
  ];

  system.stateVersion = "22.11";

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
    "anytype"
    "anytype-heart"
    "discord"
    "unrar"
  ];
}
