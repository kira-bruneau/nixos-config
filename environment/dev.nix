{ config, pkgs, ... }:

{
  imports = [
    ../feature/text-editor.nix
  ];

  environment.systemPackages = with pkgs; [
    curl
    git
    nodejs
    pkgconfig
    ripgrep
    wget
  ];
}
