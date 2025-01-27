{
  inputs,
  config,
  lib,
  ...
}:

{
  imports = [ inputs.impermanence.nixosModules.impermanence ];

  fileSystems."/persist".neededForBoot = true;

  environment.persistence."/persist" = {
    hideMounts = true;
    directories =
      [
        "/var/db"
        "/var/lib/nix"
        "/var/lib/nixos"
        "/var/lib/systemd"
        "/var/log/journal"
      ]
      ++ lib.optional config.services.accounts-daemon.enable "/var/lib/AccountsService"
      ++ lib.optional (config.security.acme.certs != { }) "/var/lib/acme"
      ++ lib.optional config.services.blueman.enable "/var/lib/blueman"
      ++ lib.optional config.hardware.bluetooth.enable "/var/lib/bluetooth"
      ++ lib.optional config.services.colord.enable "/var/lib/colord"
      ++ lib.optional config.services.dnscrypt-proxy2.enable "/var/cache/private/dnscrypt-proxy"
      ++ lib.optional config.services.fprintd.enable "/var/lib/fprint"
      ++ lib.optionals config.services.fwupd.enable [
        "/var/cache/fwupd"
        "/var/lib/fwupd"
      ]
      ++ lib.optional config.services.headscale.enable "/var/lib/headscale"
      ++ lib.optional config.services.home-assistant.enable config.services.home-assistant.configDir
      ++ lib.optionals config.services.jellyfin.enable (
        with config.services.jellyfin;
        [
          dataDir
          cacheDir
          logDir
        ]
      )
      ++ lib.optional config.services.jellyseerr.enable "/var/lib/private/jellyseerr"
      ++ lib.optional config.networking.wireless.iwd.enable "/var/lib/iwd"
      ++ lib.optional config.services.klipper.enable config.services.klipper.configDir
      ++ lib.optional config.services.kubo.enable config.services.kubo.dataDir
      ++ lib.optionals config.services.mastodon.enable [
        "/var/cache/mastodon"
        "/var/lib/mastodon"
      ]
      ++ lib.optional config.services.matrix-synapse.enable config.services.matrix-synapse.dataDir
      ++ lib.optional config.services.moonraker.enable config.services.moonraker.stateDir
      ++ lib.optionals config.networking.networkmanager.enable [
        "/etc/NetworkManager/system-connections"
        "/var/lib/NetworkManager"
        "/var/lib/NetworkManager-fortisslvpn"
      ]
      ++ lib.optionals config.services.nginx.enable [
        "/var/cache/nginx"
        "/var/log/nginx"
      ]
      ++ builtins.map (container: "/var/lib/nixos-containers/${container}") (
        builtins.attrNames config.containers
      )
      ++ lib.optional config.services.octoprint.enable config.services.octoprint.stateDir
      ++ lib.optional config.services.ollama.enable "/var/lib/private/ollama"
      ++ lib.optional config.services.opendkim.enable "/var/lib/opendkim"
      ++ lib.optional config.services.postfix.enable "/var/lib/postfix"
      ++ lib.optional config.services.postgresql.enable "/var/lib/postgresql"
      ++ lib.optional config.services.power-profiles-daemon.enable "/var/lib/power-profiles-daemon"
      ++ lib.optional config.services.prowlarr.enable "/var/lib/private/prowlarr"
      ++ lib.optional config.services.radarr.enable config.services.radarr.dataDir
      ++ lib.optional config.services.readarr.enable config.services.readarr.dataDir
      ++ lib.optional (
        config.services.mastodon.enable && config.services.mastodon.redis.createLocally
      ) "/var/lib/redis-mastodon"
      ++ lib.optional config.services.sonarr.enable config.services.sonarr.dataDir
      ++ lib.optional (config.services.tailscale.enable) "/var/lib/tailscale"
      ++ lib.optional config.services.upower.enable "/var/lib/upower"
      ++ lib.optional config.networking.wireguard.enable "/var/lib/wireguard";

    files = [
      "/etc/machine-id" # unique id (generated by systemd-boot)
    ];
  };

  services.logrotate.enable = false;
}
