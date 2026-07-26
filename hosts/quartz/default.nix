{ config, pkgs, ... }:

{
  imports = [
    ../../environments/dev.nix
    ../../environments/gaming.nix
    ../../environments/gui/sway.nix
    ../../environments/media-server.nix
    ../../services/firefox-syncserver.nix
    ../../services/habitica.nix
    ../../services/home-assistant.nix
    ../../services/kubo.nix
    ../../services/llama-cpp.nix
    ../../services/minecraft/aoc-aeronautics
    ../../services/nginx.nix
    ../../services/synapse.nix
    ../../users/builder.nix
    ../../users/kira.nix
    ./homepage-dashboard.nix
  ];

  system.stateVersion = "24.05";

  users.defaultUser = "kira";

  programs = {
    steam.localNetworkGameTransfers.openFirewall = true;
  };

  services = {
    audiobookshelf.host = "unix//run/audiobookshelf/socket";

    firefox-syncserver.singleNode.url = "https://firefox-syncserver.jakira.space";

    habitica.hostName = "habitica.jakira.space";

    home-assistant.config.homeassistant.auth_providers = {
      type = "trusted_networks";
      allow_bypass_login = true;
      trusted_networks = [
        "127.0.0.1"
        "100.64.0.0/10" # tailscale
      ];
    };

    llama-cpp.host = "/run/llama-cpp/llama-cpp.sock";

    matrix-synapse.settings = {
      server_name = "jakira.space";
      public_baseurl = "https://matrix.jakira.space";
    };

    nginx.virtualHosts =
      let
        sharedSettings = {
          recommendedProxySettings = true;
          extraConfig = ''
            allow 127.0.0.1;
            allow 100.64.0.0/10; # tailscale
            deny all;
          '';
        };
      in
      {
        "books.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://unix:/run/audiobookshelf/socket";
          proxyWebsockets = true;
        };
        "bot.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://unix:/run/llama-cpp/llama-cpp.sock";
          proxyWebsockets = true;
        };
        "habitica.jakira.space".locations."/" = sharedSettings;
        "home.jakira.space".locations."/" = sharedSettings // {
          # homepage doesn't support unix sockets (it uses nodejs's http server, but only takes a numeric listen port)
          proxyPass = "http://127.0.0.1:${toString config.services.homepage-dashboard.listenPort}";
        };
        "home-assistant.jakira.space".locations."/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.home-assistant.config.http.server_port}";
          proxyWebsockets = true;
        };
        "jellyfin.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://unix:/run/jellyfin/socket";
        };
        "prowlarr.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://127.0.0.1:${toString config.services.prowlarr.settings.server.port}";
        };
        "qbittorrent.jakira.space".locations."/" = sharedSettings // {
          # qbittorrent doesn't support unix sockets: https://github.com/qbittorrent/qBittorrent/issues/14763
          proxyPass = "http://127.0.0.1:8000";
        };
        "radarr.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://127.0.0.1:${toString config.services.radarr.settings.server.port}";
        };
        "seerr.jakira.space".locations."/" = sharedSettings // {
          # seerr doesn't support unix sockets (it uses express, but only takes a numeric listen port)
          proxyPass = "http://127.0.0.1:${toString config.services.seerr.port}";
        };
        "sonarr.jakira.space".locations."/" = sharedSettings // {
          # sonarr doesn't support unix sockets: https://github.com/Sonarr/Sonarr/issues/4427
          proxyPass = "http://127.0.0.1:${toString config.services.sonarr.settings.server.port}";
        };
      };

    postgresql.package = pkgs.postgresql_18;
  };

  systemd.services = {
    audiobookshelf.serviceConfig.RuntimeDirectory = "audiobookshelf";
    llama-cpp.serviceConfig = {
      Group = [ config.services.nginx.group ];
      RuntimeDirectory = "llama-cpp";
      UMask = "0007";
    };
    jellyfin = {
      serviceConfig.RuntimeDirectory = "jellyfin";
      environment = {
        JELLYFIN_kestrel__socket = "true";
        JELLYFIN_kestrel__socketPath = "/run/jellyfin/socket";
        JELLYFIN_kestrel__socketPermissions = "0666";
      };
    };
  };

  networking.firewall = {
    allowedTCPPorts = [
      15982 # qbittorrent
      25565 # aoc-aeronautics
      config.services.firefox-syncserver.settings.port
      config.services.forgejo.settings.server.HTTP_PORT
    ]
    ++ map ({ port, ... }: port) config.services.matrix-synapse.settings.listeners;

    allowedUDPPorts = [
      15982 # qbittorrent
      25565 # aoc-aeronautics
    ];
  };
}
