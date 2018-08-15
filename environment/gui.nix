{ config, pkgs, ... }:

{
  imports = [
    ./minimal.nix
    ../feature/display-manager.nix
    ../feature/window-manager.nix
    ../feature/web-browser.nix
  ];

  environment.systemPackages = with pkgs; [
    keepassxc
    speedcrunch
  ];
}
