{ lib, pkgs, ... }:

{
  imports = [
    ../environment/cli.nix
    ../environment/gui.nix
    ../package/arctype
  ];

  home.packages = with pkgs; [
    vscodium
  ];

  services.syncthing.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "arctype"
    "unrar"
  ];

  home.stateVersion = "21.11";
}
