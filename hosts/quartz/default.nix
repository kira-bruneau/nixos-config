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
    ../../services/minecraft/aoc-aeronautics
    ../../services/nginx.nix
    ../../services/ollama.nix
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
    audiobookshelf.port = 8001;

    firefox-syncserver.singleNode.url = "https://firefox-syncserver.jakira.space";

    habitica.hostName = "habitica.jakira.space";

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
          proxyPass = "http://127.0.0.1:${toString config.services.audiobookshelf.port}";
          proxyWebsockets = true;
        };
        "habitica.jakira.space".locations."/" = sharedSettings;
        "home.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://127.0.0.1:${toString config.services.homepage-dashboard.listenPort}";
        };
        "home-assistant.jakira.space".locations."/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.home-assistant.config.http.server_port}";
          proxyWebsockets = true;
        };
        "jellyfin.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://127.0.0.1:8096";
        };
        "prowlarr.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://127.0.0.1:${toString config.services.prowlarr.settings.server.port}";
        };
        "qbittorrent.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://127.0.0.1:8000";
        };
        "radarr.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://127.0.0.1:${toString config.services.radarr.settings.server.port}";
        };
        "seerr.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://127.0.0.1:${toString config.services.seerr.port}";
        };
        "sonarr.jakira.space".locations."/" = sharedSettings // {
          proxyPass = "http://127.0.0.1:${toString config.services.sonarr.settings.server.port}";
        };
      };

    postgresql.package = pkgs.postgresql_18;
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
