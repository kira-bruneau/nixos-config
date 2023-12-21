{
  services.tailscale = {
    enable = true;
    openFirewall = true;
  };

  systemd.services.tailscaled.environment = {
    TS_NO_LOGS_NO_SUPPORT = "true";
  };

  networking.hosts = {
    "100.64.0.1" = [ "quartz" ];
    "100.64.0.2" = [ "aurora" ];
  };
}
