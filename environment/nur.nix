{ config, pkgs, ... }:

let
  nur = import <nur> {
    inherit pkgs;
  };
in rec {
  imports = [
    ../cachix.nix
    nur.repos.metadark.modules.bluetooth-autoconnect
    nur.repos.metadark.modules.lightdm-webkit2-greeter
    nur.repos.metadark.modules.xpadneo
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    inherit nur;
  };
}
