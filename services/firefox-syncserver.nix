{ pkgs, ... }:

{
  services.mysql.package = pkgs.mariadb;

  services.firefox-syncserver = {
    enable = true;
    secrets = "/var/lib/private/firefox-syncserver/secrets";
    singleNode = {
      enable = true;
      enableNginx = true;
      hostname = "firefox.jakira.space";
    };
  };

  systemd.services.firefox-syncserver.serviceConfig.StateDirectory = "firefox-syncserver";
}
