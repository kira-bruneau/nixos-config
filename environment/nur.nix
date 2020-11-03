{ config, pkgs, ... }:

let
  nur-no-pkgs = import <nur> { pkgs = null; };
  nur = import <nur> { inherit pkgs; };
in rec {
  imports = [
    ../cachix.nix
    nur-no-pkgs.repos.metadark.modules.bluetooth-autoconnect
    nur-no-pkgs.repos.metadark.modules.lightdm-webkit2-greeter
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    inherit nur;
  };
}
