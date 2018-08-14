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

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us,us";
    xkbVariant = "colemak,";
    xkbOptions = "grp:win_space_toggle";
  };
}
