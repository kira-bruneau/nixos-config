{ config, lib, ... }:

lib.mkIf (!config.networking.networkmanager.enable) {
  networking = {
    dhcpcd.enable = false;
    useDHCP = false;
  };

  systemd.network.enable = true;
}
