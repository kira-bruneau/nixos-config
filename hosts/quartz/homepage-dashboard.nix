{ config, ... }:

{
  services.homepage-dashboard = {
    enable = true;
    allowedHosts = "*";

    services = [
      {
        "Public" = [
          {
            "Jellyfin" = {
              icon = "jellyfin.svg";
              href = "http://jellyfin.jakira.space";
            };
          }
          {
            "Seerr" = {
              icon = "seerr.svg";
              href = "http://seerr.jakira.space";
            };
          }
          {
            "Minecraft" = {
              icon = "minecraft.svg";
              widget = {
                type = "minecraft";
                url = "udp://100.64.0.12:25565";
                fields = [
                  "players"
                  "status"
                ];
              };
            };
          }
        ];
      }
      {
        "Internal" = [
          {
            "Sonarr" = {
              icon = "sonarr.svg";
              href = "http://sonarr.jakira.space";
              widget = {
                type = "sonarr";
                url = "http://localhost:${toString config.services.sonarr.settings.server.port}";
                key = "00000000000000000000000000000000";
                # enableQueue = true;
              };
            };
          }
          {
            "Radarr" = {
              icon = "radarr.svg";
              href = "http://radarr.jakira.space";
              widget = {
                type = "radarr";
                url = "http://localhost:${toString config.services.radarr.settings.server.port}";
                key = "00000000000000000000000000000000";
                # enableQueue = true;
              };
            };
          }
          {
            "Prowlarr" = {
              icon = "prowlarr.svg";
              href = "http://prowlarr.jakira.space";
              widget = {
                type = "prowlarr";
                url = "http://localhost:${toString config.services.prowlarr.settings.server.port}";
                key = "00000000000000000000000000000000";
              };
            };
          }
          {
            "qBittorrent" = {
              icon = "qbittorrent.svg";
              href = "http://qbittorrent.jakira.space";
              widget = {
                type = "qbittorrent";
                url = "http://localhost:8000";
              };
            };
          }
        ];
      }
      {
        "Calendar" = [
          {
            "" = {
              widget = {
                type = "calendar";
                firstDayInWeek = "sunday";
                view = "monthly";
                showTime = true;
                integrations = [
                  {
                    type = "sonarr";
                    service_group = "Internal";
                    service_name = "Sonarr";
                    params = {
                      unmonitored = true;
                    };
                  }
                  {
                    type = "radarr";
                    service_group = "Internal";
                    service_name = "Radarr";
                    params = {
                      unmonitored = true;
                    };
                  }
                ];
              };
            };
          }
        ];
      }
    ];

    widgets = [
      {
        resources = {
          cpu = true;
          memory = true;
          expanded = true;
          disk = [
            "/persist"
            "/srv/media-ssd"
          ];
        };
      }
    ];
  };
}
