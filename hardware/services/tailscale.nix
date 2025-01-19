{ lib, config, ... }:

let
  cfg = config.services.tailscale;
in
{
  services.tailscale = {
    enable = lib.mkDefault true;
    openFirewall = true;
  };

  systemd = lib.mkIf cfg.enable {
    services.tailscaled.environment = {
      TS_NO_LOGS_NO_SUPPORT = "true";
    };

    network.wait-online.ignoredInterfaces = [ config.services.tailscale.interfaceName ];
  };

  networking.hosts = lib.mkIf cfg.enable {
    "100.64.0.4" = [ "luna" ];
  };
}
