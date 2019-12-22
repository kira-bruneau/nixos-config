{ config, pkgs, ... }:

{
  imports = [
    ../environment/cli.nix
    ../environment/gaming.nix
    ../environment/gui.nix
  ];

  services.syncthing.enable = true;
}
