{ config, lib, ... }:

lib.mkIf (!config.networking.networkmanager.enable) {
  networking.useDHCP = false;
  systemd.network.enable = true;
}
