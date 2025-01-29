{ config, lib, ... }:

{
  services.mautrix-whatsapp = {
    enable = true;

    settings = lib.mkForce {
      bridge = {
        permissions = {
          "*" = "relay";
          "jakira.space" = "user";
          "@kira:jakira.space" = "admin";
        };
      };

      database = {
        type = "postgres";
        uri = "postgresql:///mautrix-whatsapp?host=/var/run/postgresql";
      };

      homeserver = {
        address = "http://localhost:8008";
        domain = config.services.matrix-synapse.settings.server_name;
        async_media = true;
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
        name = "mautrix-whatsapp";
        ensureDBOwnership = true;
      }
    ];

    ensureDatabases = [ "mautrix-whatsapp" ];
  };
}
