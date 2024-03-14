{ lib, ... }:

{
  imports = [
    ../environments/gui/sway.nix
    ../users/kira.nix
  ];

  system.stateVersion = "22.11";
}
