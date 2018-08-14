{ config, pkgs, ... }:

{
  imports = [
    ../feature/text-editor.nix
  ];

  environment.systemPackages = with pkgs; [
    binutils
    clang
    gcc
    git
    gnumake
    nodejs
    pkgconfig
  ];
}
