{ config, pkgs, ... }:

{
  imports = [
    ../feature/text-editor.nix
  ];

  environment.systemPackages = with pkgs; [
    binutils
    clang
    cloc
    cmake
    gcc
    git
    gnumake
    ninja
    nodejs
    pkgconfig
    python2
    python3
  ];
}
