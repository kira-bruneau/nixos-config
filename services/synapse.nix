{
  config,
  lib,
  pkgs,
  ...
}:

let
  settings = config.services.matrix-synapse.settings;
  dbName = settings.database.args.database;
  dbUser = settings.database.args.user;
in
{
  imports = [
    ./mautrix-discord.nix
    ./mautrix-whatsapp.nix
  ];

  services = {
    matrix-synapse = {
      enable = true;

      settings = {
        server_name = "jakira.space";
        public_baseurl = "https://matrix.jakira.space";
        listeners = [
          {
            port = 8008;
            bind_addresses = [ "0.0.0.0" ];
            type = "http";
            tls = false;
            x_forwarded = true;
            resources = [
              {
                names = [
                  "client"
                  "federation"
                ];

                compress = true;
              }
            ];
          }
        ];

        forgotten_room_retention_period = "28d";
        media_retention.remote_media_lifetime = "14d";
      };
    };

    postgresql.enable = true;
  };

  networking.firewall.allowedTCPPorts = [ 8008 ];

  systemd.services.postgresql.postStart = lib.mkAfter ''
    psql -f ${pkgs.writeText "matrix-synapse-init.sql" ''
      CREATE ROLE "${dbUser}";
      CREATE DATABASE "${dbName}" WITH OWNER "${dbUser}"
        TEMPLATE template0
        LC_COLLATE = "C"
        LC_CTYPE = "C";
    ''}
  '';
}
