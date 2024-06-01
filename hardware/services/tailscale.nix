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
    "100.64.0.1" = [ "quartz" ];
    "100.64.0.2" = [ "aurora" ];
    "100.64.0.3" = [ "steamdeck" ];
    "100.64.0.4" = [ "luna" ];
    "100.64.0.5" = [ "jackflix" ];
    "100.64.0.6" = [ "amethyst" ];
  };
}
