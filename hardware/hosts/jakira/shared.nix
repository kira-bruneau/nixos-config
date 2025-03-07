{ config, lib, ... }:

{
  nix.settings.trusted-public-keys = [ "jakira:wNeIHrxXz2JonyeKJ8Dx6YThMAt1oTn58kVCOAYR1JI=" ];

  networking = {
    hosts."209.38.0.183" = [
      "jakira"
      "jakira.space"
      "chat.jakira.space"
      "headscale.jakira.space"
      "mastodon.jakira.space"
      "matrix.jakira.space"
    ];

    wireguard.interfaces.wg0 = {
      ips = lib.optional (config.networking.hostName == "jakira") "10.100.0.1/32";
      listenPort = if config.networking.hostName == "jakira" then 51820 else null;
      peers = lib.optional (config.networking.hostName != "jakira") {
        name = "jakira";
        publicKey = "nliKU5Ry6/NKRkWWEQs9HkSs6micz4kbw7HuO26XskM=";
        allowedIPs = [ "10.100.0.1/32" ];
        endpoint = "jakira:51820";
        persistentKeepalive = 25;
      };
    };

    firewall.allowedUDPPorts = lib.optional (config.networking.hostName == "jakira") 51820;
  };

  programs.ssh.knownHosts.jakira.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKGA1ahkvVffvxr4l/tXLOxOAa4RnenIY5PQjx0D+BaO";
}
