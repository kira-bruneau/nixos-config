{ config, pkgs, ... }:

{
  imports = [ ../../environments/config.nix ];

  home = {
    packages = with pkgs; [ rmg ];
  };
}
