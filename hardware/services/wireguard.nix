{ config, lib, ... }:

let
  cfg = config.networking.wireguard;
in
{
  networking.wireguard = {
    enable = builtins.any (interface: (builtins.length interface.ips) > 0) (
      builtins.attrValues cfg.interfaces
    );

    interfaces.wg0 = {
      privateKeyFile = "/var/lib/wireguard/wg0_key";
      generatePrivateKeyFile = true;
    };
  };

  systemd.network.wait-online.ignoredInterfaces = lib.mkIf cfg.enable [ "wg0" ];
}
