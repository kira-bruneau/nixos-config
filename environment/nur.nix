{ config, pkgs, ... }:

let
  nur = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/NUR/archive/02b1fdc4d33180d0f9cf03e7249353b75236a59e.tar.gz";
    sha256 = "04g9c78wjgqcj7h0gzlbdghkzc2mzq3iyl2p9ljzn5jjmi94yf67";
  }) {
    inherit pkgs;
  };
in rec {
  imports = [
    ../cachix.nix
    nur.repos.metadark.modules.lightdm-webkit2-greeter
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    inherit nur;
  };
}
