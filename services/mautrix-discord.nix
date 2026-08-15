{ lib, pkgs, ... }:

{
  services.mautrix-discord = {
    enable = true;

    settings = {
      homeserver.async_media = true;

      appservice.database = {
        type = "postgres";
        uri = "postgresql:///mautrix-discord?host=/var/run/postgresql";
      };

      bridge = {
        permissions = {
          "*" = "relay";
          "jakira.space" = "user";
          "@jack:jakira.space" = "admin";
          "@kira:jakira.space" = "admin";
        };

        login_shared_secret_map."jakira.space" = "$DOUBLE_PUPPET_SECRET_JAKIRA";

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

      logging.writers = [ { type = "journald"; } ];
    };
  };

  systemd.services.mautrix-discord-registration = {
    serviceConfig.IgnoreSIGPIPE = false; # https://stackoverflow.com/a/44376786
    script = lib.mkBefore ''
      if [ -e /var/lib/mautrix-discord/config.yaml ]; then
        DOUBLE_PUPPET_SECRET_JAKIRA="$(${pkgs.yq}/bin/yq -er '.bridge.login_shared_secret_map."jakira.space"' /var/lib/mautrix-discord/config.yaml)"
      fi

      export DOUBLE_PUPPET_SECRET_JAKIRA
    '';
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
