{ config, pkgs, ... }:

{
  # TODO: Install cocktail
  environment.systemPackages = with pkgs; [
    apacheAnt
    binutils
    bison
    clang
    gcc
    git
    gnumake
    javaPackages.junit_4_12
    jdk7
    python2Full
    unzip

    # Packages not necessary for building Klokwork
    jetbrains.idea-community
    virtualbox
  ];
}
