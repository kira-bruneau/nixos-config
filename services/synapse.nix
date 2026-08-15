{ config, ... }:

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

  services.matrix-synapse = {
    enable = true;
    settings = {
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

      app_service_config_files = [ "/var/lib/matrix-synapse/services/doublepuppet.yaml" ];
      forgotten_room_retention_period = "28d";
      media_retention.remote_media_lifetime = "14d";
      default_room_version = "12";
    };
  };

  systemd.services.matrix-synapse = {
    serviceConfig.IgnoreSIGPIPE = false; # https://stackoverflow.com/a/44376786
    preStart = ''
      mkdir -p /var/lib/matrix-synapse/services
      if [ ! -e /var/lib/matrix-synapse/services/doublepuppet.yaml ]; then
        cat << EOF > /var/lib/matrix-synapse/services/doublepuppet.yaml
      id: doublepuppet
      url:
      as_token: $(tr -dc A-Za-z0-9 < /dev/urandom | head -c 64)
      hs_token: $(tr -dc A-Za-z0-9 < /dev/urandom | head -c 64)
      sender_localpart: $(tr -dc A-Za-z0-9 < /dev/urandom | head -c 64)
      rate_limited: false
      namespaces:
        users:
        - regex: '@.*:${config.services.matrix-synapse.settings.server_name}'
          exclusive: false
      EOF
      fi
    '';
  };

  services.postgresql = {
    enable = true;
    ensureUsers = [ { name = dbUser; } ];
  };

  systemd.services.postgresql-setup.script = ''
    psql -tAc "SELECT 1 FROM pg_database WHERE datname = '${dbName}'" | grep -q 1 || \
      psql -tAc 'CREATE DATABASE "${dbName}" OWNER "${dbUser}" TEMPLATE template0 LC_COLLATE = "C" LC_CTYPE = "C"'
  '';

  services.mautrix-discord.settings.homeserver = {
    address = "http://localhost:${toString (builtins.head config.services.matrix-synapse.settings.listeners).port}";
    domain = config.services.matrix-synapse.settings.server_name;
  };

  services.mautrix-whatsapp.settings.homeserver = {
    address = "http://localhost:${toString (builtins.head config.services.matrix-synapse.settings.listeners).port}";
    domain = config.services.matrix-synapse.settings.server_name;
  };
}
