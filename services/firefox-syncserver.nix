{
  config,
  lib,
  pkgs,
  ...
}:

{
  services.mysql.package = pkgs.mariadb;

  services.firefox-syncserver = {
    enable = true;
    secrets = "/var/lib/private/firefox-syncserver/secrets";
    logLevel = "trace";

    singleNode = {
      enable = true;
      hostname = lib.mkDefault "localhost";
      capacity = 1;
    };

    settings.host = "0.0.0.0";
  };

  systemd.services = {
    firefox-syncserver.serviceConfig.StateDirectory = "firefox-syncserver";

    firefox-syncserver-pre-setup = {
      wantedBy = [ "firefox-syncserver.service" ];
      requires = [ "firefox-syncserver.service" ];
      before = [ "firefox-syncserver.service" ];

      unitConfig = {
        ConditionPathExists = "!${config.services.firefox-syncserver.secrets}";
      };

      serviceConfig = {
        StateDirectory = "firefox-syncserver";
        DynamicUser = true;
        IgnoreSIGPIPE = false; # https://stackoverflow.com/a/44376786
      };

      script = ''
        echo "SYNC_MASTER_SECRET=$(tr -dc A-Za-z0-9 < /dev/urandom | head -c 64)" \
          >> ${lib.escapeShellArg config.services.firefox-syncserver.secrets}
      '';
    };
  };
}
