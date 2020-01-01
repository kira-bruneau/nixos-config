{ config, pkgs, ... }:

let
  nur = builtins.fetchTarball {
    url = "https://github.com/nix-community/NUR/archive/c1fbd5f1901322935da00d17c6dc09a92b90bb10.tar.gz";
    sha256 = "0940qcsflnaljgxjy84b6amc07zy4g5lpb3jyy2vjaj9p5r2ldq6";
  };
in {
  imports = [ ../cachix.nix ];
  nixpkgs.config.packageOverrides = pkgs: {
    nur = import nur { inherit pkgs; };
  };
}
