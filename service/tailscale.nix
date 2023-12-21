{
  services.tailscale = {
    enable = true;
    openFirewall = true;
  };

  networking.hosts = {
    "100.64.0.1" = [ "quartz" ];
    "100.64.0.2" = [ "aurora" ];
  };
}
