{
  config,
  lib,
  pkgs,
  ...
}:

{
  services.mautrix-whatsapp = {
    enable = true;

    settings = {
      network.history_sync.request_full_sync = true;

      bridge.permissions = {
        "*" = "relay";
        "jakira.space" = "user";
        "@kira:jakira.space" = "admin";
      };

      database = {
        type = "postgres";
        uri = "postgresql:///mautrix-whatsapp?host=/var/run/postgresql";
      };

      homeserver.async_media = true;

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
        pickle_key = "$ENCRYPTION_PICKLE_KEY";
      };

      logging.writers = [ { type = "journald"; } ];
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

  systemd.services.mautrix-whatsapp = {
    serviceConfig.IgnoreSIGPIPE = false; # https://stackoverflow.com/a/44376786
    preStart = lib.mkBefore ''
      if [ -e /var/lib/mautrix-whatsapp/config.yaml ]; then
        export ENCRYPTION_PICKLE_KEY=$(${pkgs.yq}/bin/yq -er .encryption.pickle_key /var/lib/mautrix-whatsapp/config.yaml)
      fi

      if [ -z "$ENCRYPTION_PICKLE_KEY" ]; then
        export ENCRYPTION_PICKLE_KEY=$(tr -dc A-Za-z0-9 < /dev/urandom | head -c 64)
      fi
    '';
  };
}
