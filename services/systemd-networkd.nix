{ config, lib, ... }:

lib.mkIf (!config.networking.networkmanager.enable) {
  networking = {
    dhcpcd.enable = false;
    useDHCP = false;
  };

  systemd.network = {
    enable = true;

    networks.predicatable-ethernet = {
      matchConfig.Name = "en*";
      networkConfig.DHCP = "yes";
    };

    networks.predictable-wifi = {
      matchConfig.Name = "wl*";
      networkConfig.DHCP = "yes";
    };

    networks.legacy-ethernet = {
      matchConfig.Name = "eth*";
      networkConfig.DHCP = "yes";
    };

    networks.legacy-wifi = {
      matchConfig.Name = "wlan*";
      networkConfig.DHCP = "yes";
    };
  };
}
