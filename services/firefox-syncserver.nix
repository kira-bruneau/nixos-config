{ config, pkgs, ... }:

{
  services.mysql.package = pkgs.mariadb;

  services.firefox-syncserver = {
    enable = true;
    secrets = "/var/lib/private/firefox-syncserver/secrets";

    singleNode = {
      enable = true;
      url = "https://firefox-syncserver.jakira.space";
    };

    settings.host = "0.0.0.0";
  };

  systemd.services.firefox-syncserver.serviceConfig.StateDirectory = "firefox-syncserver";

  networking.firewall.allowedTCPPorts = [ config.services.firefox-syncserver.settings.port ];
}
