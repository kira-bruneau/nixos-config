{ config, ... }:

{
  services.tailscale = {
    enable = true;
    openFirewall = true;
  };

  systemd = {
    services.tailscaled.environment = {
      TS_NO_LOGS_NO_SUPPORT = "true";
    };

    network.wait-online.ignoredInterfaces = [ config.services.tailscale.interfaceName ];
  };

  networking.hosts = {
    "100.64.0.4" = [ "luna" ];
  };
}
