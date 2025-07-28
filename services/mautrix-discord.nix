{ lib, config, ... }:

{
  services.mautrix-discord = {
    enable = true;

    settings = lib.mkForce {
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

      bridge = {
        permissions = {
          "*" = "relay";
          "jakira.space" = "user";
          "@jack:jakira.space" = "admin";
          "@kira:jakira.space" = "admin";
        };

        encryption = {
          allow = true;
          default = true;
          require = true;

          # Recommended options from mautrix documentation
          # for additional security.
          delete_keys = {
            dont_store_outbound = true;
            ratchet_on_decrypt = true;
            delete_fully_used_on_decrypt = true;
            delete_prev_on_new_session = true;
            delete_on_device_delete = true;
            periodically_delete_expired = true;
            delete_outdated_inbound = true;
          };

          verification_levels = {
            receive = "cross-signed-tofu";
            send = "cross-signed-tofu";
            share = "cross-signed-tofu";
          };
        };
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
