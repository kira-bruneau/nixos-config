{ config, pkgs, ... }:

let
  nur = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/NUR/archive/d2317cc7fbc5a07fec3491a1d87d64a03b0d8291.tar.gz";
    sha256 = "1hasha7ggz3zf6jxafj62fbm3l7bcqzpm71jdjsh57p7wzz8fx80";
  }) {
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
