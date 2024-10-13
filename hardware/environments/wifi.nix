{ config, ... }:

{
  hardware.wirelessRegulatoryDatabase = true;

  networking.wireless.iwd.enable =
    !config.networking.wireless.enable && !config.networking.networkmanager.enable;

  programs.captive-browser.enable = true;
}
