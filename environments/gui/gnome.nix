{ pkgs, ... }:

{
  imports = [ ./. ];

  services = {
    xserver.excludePackages = with pkgs; [ xterm ];
    desktopManager.gnome.enable = true;
    gnome.core-apps.enable = false;
  };

  environment.gnome.excludePackages = with pkgs; [ gnome-tour ];

  networking.networkmanager = {
    wifi.backend = "iwd";

    # iwd autoconnect doesn't work
    # https://wiki.archlinux.org/title/NetworkManager#Using_iwd_as_the_Wi-Fi_backend:~:text=This%20may%20not%20work%20on%20some%20configurations%2E
    settings.device."wifi.iwd.autoconnect" = false;
  };
}
