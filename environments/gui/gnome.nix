{ pkgs, ... }:

{
  imports = [ ./. ];

  services = {
    xserver.excludePackages = with pkgs; [ xterm ];
    desktopManager.gnome.enable = true;
    gnome.core-apps.enable = false;
  };

  environment.gnome.excludePackages = with pkgs; [ gnome-tour ];
  networking.networkmanager.wifi.backend = "iwd";
}
