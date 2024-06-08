{ pkgs, ... }:

{
  services.dnscrypt-proxy2 = {
    enable = true;
    settings = {
      listen_addresses = [
        "127.0.0.1:53"
        "[::1]:53"
      ];

      disabled_server_names = [ "cs-montreal" ];
      doh_servers = false;
      require_dnssec = true;

      anonymized_dns = {
        routes = [
          {
            server_name = "*";
            via = [ "anon-cs-montreal" ];
          }
        ];

        skip_incompatible = true;
      };

      sources = {
        public-resolvers = {
          urls = [
            "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
            "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
            "https://ipv6.download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
          ];

          cache_file = "/var/cache/dnscrypt-proxy/public-resolvers.md";
          minisign_key = "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
        };

        relays = {
          urls = [
            "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/relays.md"
            "https://download.dnscrypt.info/resolvers-list/v3/relays.md"
            "https://ipv6.download.dnscrypt.info/resolvers-list/v3/relays.md"
          ];

          cache_file = "/var/cache/dnscrypt-proxy/relays.md";
          minisign_key = "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
        };
      };

      forwarding_rules = pkgs.writeText "forwarding-rules.txt" ''
        lan 192.168.1.1
      '';
    };
  };

  # Use dnscrypt-proxy as nameserver instead of dhcpcd or systemd-resolved
  networking = {
    nameservers = [
      "127.0.0.1"
      "::1"
    ];

    dhcpcd.extraConfig = "nohook resolv.conf";
  };

  services.resolved.enable = false;
}
