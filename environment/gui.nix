{ config, pkgs, ... }:

{
  # Services
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    windowManager.i3.enable = true;
  };

  # Packages
  environment.systemPackages = with pkgs; [
    chromium
    firefox
    keepassxc
    speedcrunch
  ];
}
