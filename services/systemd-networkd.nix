{ config, lib, ... }:

lib.mkIf (!config.networking.networkmanager.enable) {
  networking = {
    dhcpcd.enable = false;
    useDHCP = false;
  };

  systemd.network = {
    enable = true;

    networks.en = {
      matchConfig.Name = "en*";
      networkConfig.DHCP = "yes";
    };

    networks.eth = {
      matchConfig.Name = "eth*";
      networkConfig.DHCP = "yes";
    };

    networks.wlan = {
      matchConfig.Name = "wlan*";
      networkConfig.DHCP = "yes";
    };
  };
}
