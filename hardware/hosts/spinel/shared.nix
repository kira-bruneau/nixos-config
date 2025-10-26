{ config, lib, ... }:

{
  networking = {
    wireguard.interfaces.wg0 = {
      ips = lib.optional (config.networking.hostName == "spinel") "10.100.0.3/32";
      peers = lib.optional (config.networking.hostName != "spinel") {
        name = "spinel";
        publicKey = "HSkbreHI9aLAAA6opgwdCXKER6ycfWbeYmGqqxqGRSM=";
        allowedIPs = [ "10.100.0.3/32" ];
      };
    };

    hosts = {
      "10.100.0.3" = lib.mkIf config.networking.wireguard.enable [ "spinel" ];
    };
  };
}
