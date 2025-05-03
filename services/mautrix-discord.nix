{ config, ... }:

{
  services.mautrix-discord = {
    enable = true;

    settings = {
      homeserver = {
        address = "http://localhost:8008";
        domain = config.services.matrix-synapse.settings.server_name;
        async_media = true;
      };

      appservice = {
        database = {
          type = "postgres";
          uri = "postgresql:///mautrix-discord?host=/var/run/postgresql";
        };
      };

      bridge.permissions = {
        "*" = "relay";
        "jakira.space" = "user";
        "@kira:jakira.space" = "admin";
      };

      logging.writers = [
        {
          type = "journald";
        }
      ];
    };
  };

  services.postgresql = {
    enable = true;

    ensureUsers = [
      {
        name = "mautrix-discord";
        ensureDBOwnership = true;
      }
    ];

    ensureDatabases = [ "mautrix-discord" ];
  };
}
