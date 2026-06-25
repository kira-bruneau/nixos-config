{ config, lib, ... }:

{
  networking = {
    wireguard.interfaces.wg0 = {
      ips = lib.optional (config.networking.hostName == "jasper") "10.100.0.4/32";
      peers = lib.optional (config.networking.hostName != "jasper") {
        name = "jasper";
        publicKey = "+g5erg0fSKLuLMD2UiVeLpW3RFlWLoVt03En8oRc+A8=";
        allowedIPs = [ "10.100.0.4/32" ];
      };
    };

    hosts = {
      "100.64.0.15" = lib.mkIf config.services.tailscale.enable [ "jasper" ];
      "10.100.0.4" = lib.mkIf config.networking.wireguard.enable [ "jasper" ];
    };
  };
}
