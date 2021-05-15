{ config, pkgs, ... }:

let
  nur = import <nur> {
    inherit pkgs;
  };
in rec {
  disabledModules = [
    "hardware/xpadneo.nix"
    "programs/bash/undistract-me.nix"
    "services/video/replay-sorcery.nix"
  ];

  imports = with nur.repos.metadark.modules; [
    ../cachix.nix
    bluetooth-autoconnect
    lightdm-webkit2-greeter
    replay-sorcery
    undistract-me
    xpadneo
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    inherit nur;
  };
}
