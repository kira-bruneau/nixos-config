{
  networking.wireguard = {
    enable = true;
    interfaces.wg0 = {
      privateKeyFile = "/var/lib/wireguard/wg0_key";
      generatePrivateKeyFile = true;
    };
  };

  systemd.network.wait-online.ignoredInterfaces = [ "wg0" ];
}
