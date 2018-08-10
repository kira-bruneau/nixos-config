{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    apacheAnt
    bison
    clang
    gcc
    git
    jdk
    # junit (currently broken)
    python2Full
    unzip
    virtualbox
    # TODO: intellij
  ];
}
