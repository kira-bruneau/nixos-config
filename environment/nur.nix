{ config, pkgs, ... }:

let
  nur = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/NUR/archive/f4c8dd593197c739c0ffc4b1258d80ff4239902b.tar.gz";
    sha256 = "0d8xknzi5axq5r42wi3msfdsm4nkc2772l3a7vsplabjcv02cjiq";
  }) {
    inherit pkgs;
  };
in rec {
  imports = [
    ../cachix.nix
    nur.repos.metadark.modules.lightdm-webkit2-greeter
    nur.repos.metadark.modules.xpadneo
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    inherit nur;
  };
}
