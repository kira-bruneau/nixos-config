{ config, ... }:

{
  networking.wireless.iwd.enable = !config.networking.wireless.enable && !config.networking.networkmanager.enable;
}
