{ config, ... }:

{
  hardware.wirelessRegulatoryDatabase = true;

  networking.wireless.iwd.enable =
    !config.networking.wireless.enable
    && !(
      config.networking.networkmanager.enable && config.networking.networkmanager.wifi.backend != "iwd"
    );

  programs.captive-browser.enable = true;
}
