{ config, pkgs, ... }:

{
  # TODO: Install cocktail
  environment.systemPackages = with pkgs; [
    # Packages required for building Klocwork
    apacheAnt
    binutils
    bison
    clang
    gcc
    git
    gnumake
    javaPackages.junit_4_12
    jdk7
    python2
    unzip

    # Other packages used for Klocwork development
    git-review
    jetbrains.idea-community
    virtualbox
  ];
}
