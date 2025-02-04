{ config, lib, ... }:

{
  services.mautrix-whatsapp = {
    enable = true;

    environmentFile = "/var/lib/mautrix-whatsapp/tokens.env";

    settings = lib.mkForce {
      network.history_sync = {
        request_full_sync = true;
        full_sync_config.days_limit = 10 * 365;
      };

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

      backfill = {
        enabled = true;
        max_initial_messages = 2147483646;
        max_catchup_messages = 2147483646;
        threads.max_initial_messages = 2147483646;
      };

      encryption = {
        allow = true;
        default = true;
        require = true;
        pickle_key = "$MAUTRIX_WHATSAPP_ENCRYPTION_PICKLE_KEY";
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
