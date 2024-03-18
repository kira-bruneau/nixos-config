{ config, lib, pkgs, ... }:

{
  imports = [
    ../environments/bluetooth.nix
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
    ../environments/laptop.nix
  ];

  home.stateVersion = "23.11";
}
