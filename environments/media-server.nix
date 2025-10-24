{
  config,
  lib,
  pkgs,
  pkgsUnstable,
  ...
}:

let
  jsonFormat = pkgs.formats.json { };

  qbittorrentFormat = pkgs.formats.ini {
    listToValue = lib.concatMapStringsSep ", " (lib.generators.mkValueStringDefault { });
  };

  xmlFormat = pkgs.formats.xml { };

  downloadClients = {
    "qBittorrent" = {
      implementation = "QBittorrent";
      fields = {
        port = qBittorrent.Preferences."WebUI\\Port";
      };
    };
  };

  sonarr = {
    port = "8989";
    apiKey = "00000000000000000000000000000000";
    rootFolders = [
      { path = "/srv/media-ssd/shows"; }
    ];

    inherit downloadClients;
  };

  radarr = {
    port = "7878";
    apiKey = "00000000000000000000000000000000";
    rootFolders = [
      { path = "/srv/media-ssd/movies"; }
    ];

    inherit downloadClients;
  };

  prowlarr = {
    port = "9696";
    apiKey = "00000000000000000000000000000000";

    applications = {
      "Sonarr" = {
        syncLevel = "fullSync";
        implementation = "Sonarr";
        fields.apiKey = sonarr.apiKey;
      };
      "Radarr" = {
        syncLevel = "fullSync";
        implementation = "Radarr";
        fields.apiKey = radarr.apiKey;
      };
    };

    indexers = {
      "1337x" = {
        implementation = "Cardigann";
        fields = {
          definitionFile = "1337x";
          downloadlink = 1; # magnet
          downloadlink2 = 0; # iTorrents.org
          sort = 2; # created
          type = 1; # desc
        };
      };
      "AnimeTosho" = {
        implementation = "Torznab";
        fields = {
          baseUrl = "https://feed.animetosho.org";
        };
      };
      "LimeTorrents" = {
        implementation = "Cardigann";
        fields = {
          definitionFile = "limetorrents";
          downloadlink = 1; # magnet
          downloadlink2 = 0; # iTorrents.org
          sort = 0; # created
        };
      };
      "Solid Torrents" = {
        implementation = "Cardigann";
        fields = {
          definitionFile = "solidtorrents";
          prefer_magnet_links = true;
          sort = 0; # created
          type = 1; # desc
        };
      };
      "The Pirate Bay" = {
        implementation = "Cardigann";
        fields = {
          definitionFile = "thepiratebay";
        };
      };
      "TheRARBG" = {
        implementation = "Cardigann";
        fields = {
          definitionFile = "therarbg";
          sort = 0; # created desc
        };
      };
      "YTS" = {
        implementation = "Cardigann";
        fields = {
          definitionFile = "yts";
        };
      };
    };

    inherit downloadClients;
  };

  qBittorrent = {
    Preferences = {
      "WebUI\\Port" = 8000;
      "WebUI\\LocalHostAuth" = false;
      "WebUI\\AuthSubnetWhitelistEnabled" = true;
      "WebUI\\AuthSubnetWhitelist" = "0.0.0.0/0";
    };

    BitTorrent = {
      "Session\\DefaultSavePath" = "/srv/media-ssd/downloads";
      "Session\\Port" = 15982;
      "Session\\GlobalMaxRatio" = 2;
      "Session\\ExcludedFileNames" = [ "*.lnk" ];
    };

    Network = {
      PortForwardingEnabled = false;
    };
  };

  jellyfin = {
    mediaLibraries = {
      Shows = {
        type = "tvshows";
        folders = sonarr.rootFolders;
        options.EnableRealtimeMonitor = true;
      };

      Movies = {
        type = "movies";
        folders = radarr.rootFolders;
        options.EnableRealtimeMonitor = true;
      };

      Downloads = {
        folders = [ { path = qBittorrent.BitTorrent."Session\\DefaultSavePath"; } ];
        options = {
          EnableEmbeddedTitles = true;
          EnableRealtimeMonitor = true;
        };
      };
    };
  };

  makeCurlScript =
    name: options: ctx: requests:
    pkgs.writeTextFile {
      inherit name;
      executable = true;
      text = ''
        #!${pkgs.curl}/bin/curl -K
        ${options}
        ${builtins.concatStringsSep "\nnext\n" (builtins.map (request: "${ctx}\n${request}") requests)}
      '';
    };

  makeMediaLibrary =
    name:
    {
      type ? null,
      folders,
      options ? null,
    }:
    pkgs.runCommand name
      {
        inherit type;
        folders = builtins.map (folder: folder.path) folders;
        options =
          if options != { } then
            xmlFormat.generate "options.xml" {
              LibraryOptions = {
                "@xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance";
                "@xmlns:xsd" = "http://www.w3.org/2001/XMLSchema";
              }
              // options;
            }
          else
            null;
      }
      ''
        mkdir "$out"

        if [ -n "$type" ]; then
          touch "$out/$type.collection"
        fi

        for folder in ''${folders}; do
          name=$(basename "$folder")
          if [ -e "$out/$name.mblink" ]; then
            i=1
            while [ -e "$out/$name$i.mblink" ]; do
              i=$((i+1))
            done
            name=$name$i
          fi
          echo -n "$folder" > "$out/$name.mblink"
        done

        if [ -n "$options" ]; then
          ln -s "$options" "$out/options.xml"
        fi
      '';

  makeArrConfig =
    {
      implementation,
      fields ? [ ],
      ...
    }@config:
    {
      configContract = "${implementation}Settings";
    }
    // config
    // {
      fields = builtins.map (name: {
        inherit name;
        value = fields.${name};
      }) (builtins.attrNames fields);
    };
in
{
  services.nginx.virtualHosts =
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
      "home.jakira.space".locations."/" = sharedSettings // {
        proxyPass = "http://127.0.0.1:${toString config.services.homepage-dashboard.listenPort}";
      };
      "jellyfin.jakira.space".locations."/" = sharedSettings // {
        proxyPass = "http://127.0.0.1:8096";
      };
      "jellyseerr.jakira.space".locations."/" = sharedSettings // {
        proxyPass = "http://127.0.0.1:5055";
      };
      "prowlarr.jakira.space".locations."/" = sharedSettings // {
        proxyPass = "http://127.0.0.1:${prowlarr.port}";
      };
      "radarr.jakira.space".locations."/" = sharedSettings // {
        proxyPass = "http://127.0.0.1:${radarr.port}";
      };
      "sonarr.jakira.space".locations."/" = sharedSettings // {
        proxyPass = "http://127.0.0.1:${sonarr.port}";
      };
      "qbittorrent.jakira.space".locations."/" = sharedSettings // {
        proxyPass = "http://127.0.0.1:${toString qBittorrent.Preferences."WebUI\\Port"}";
      };
    };

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
            "Jellyseerr" = {
              icon = "jellyseerr.svg";
              href = "http://jellyseerr.jakira.space";
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
                url = "http://localhost:${sonarr.port}";
                key = sonarr.apiKey;
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
                url = "http://localhost:${radarr.port}";
                key = radarr.apiKey;
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
                url = "http://localhost:${prowlarr.port}";
                key = prowlarr.apiKey;
              };
            };
          }
          {
            "qBittorrent" = {
              icon = "qbittorrent.svg";
              href = "http://qbittorrent.jakira.space";
              widget = {
                type = "qbittorrent";
                url = "http://localhost:${toString qBittorrent.Preferences."WebUI\\Port"}";
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

  services.jellyfin = {
    enable = true;
    package = pkgsUnstable.jellyfin;
    logDir = "/var/log/jellyfin";
    openFirewall = true;
  };

  users.users.jellyfin.extraGroups = [
    "sonarr"
    "radarr"
    "qbittorrent"
  ];

  systemd.services.jellyfin = {
    serviceConfig = {
      UMask = lib.mkForce "0022";
      CapabilityBoundingSet = "";
      ProcSubset = "pid";
      ProtectClock = true;
      ProtectHome = true;
      ProtectProc = "invisible";
      ProtectSystem = "full"; # strict doesn't work: Failed to create CoreCLR, HRESULT: 0x80004005
    };

    unitConfig = {
      RequiresMountsFor = builtins.concatMap (lib: builtins.map (folder: folder.path) lib.folders) (
        builtins.attrValues jellyfin.mediaLibraries
      );
    };

    preStart = ''
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${
        xmlFormat.generate "system.xml" {
          ServerConfiguration = {
            "@xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance";
            "@xmlns:xsd" = "http://www.w3.org/2001/XMLSchema";

            IsStartupWizardCompleted = true;

            UICulture = "en-US";
            PreferredMetadataLanguage = "en";
            MetadataCountryCode = "CA";

            PluginRepositories = {
              RepositoryInfo = {
                Name = "Jellyfin Stable";
                Url = "https://repo.jellyfin.org/releases/plugin/manifest-stable.json";
                Enabled = true;
              };
            };

            LibraryMonitorDelay = 1;
          };
        }
      } "${config.services.jellyfin.configDir}/system.xml"

      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${
        xmlFormat.generate "branding.xml" {
          BrandingOptions = {
            "@xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance";
            "@xmlns:xsd" = "http://www.w3.org/2001/XMLSchema";

            CustomCss = ''
              .playedIndicator {
                display: none;
              }
            '';

            SplashscreenEnabled = true;
          };
        }
      } "${config.services.jellyfin.configDir}/branding.xml"

      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${
        xmlFormat.generate "encoding.xml" {
          EncodingOptions = {
            "@xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance";
            "@xmlns:xsd" = "http://www.w3.org/2001/XMLSchema";

            EnableHardwareEncoding = true;
            HardwareAccelerationType = "vaapi";
            VaapiDevice = "/dev/dri/by-path/pci-0000:0b:00.0-render";

            HardwareDecodingCodecs.string = [
              "av1"
              "h264"
              "hevc"
              "vp9"
            ];

            EnableDecodingColorDepth10Hevc = true;
            EnableTonemapping = true;
          };
        }
      } "${config.services.jellyfin.configDir}/encoding.xml"

      ${pkgs.coreutils}/bin/mkdir -p "${config.services.jellyfin.dataDir}/root/default"
      ${builtins.concatStringsSep "\n" (
        builtins.map (
          name:
          let
            lib = makeMediaLibrary name jellyfin.mediaLibraries.${name};
          in
          "${pkgs.coreutils}/bin/ln -sfn ${lib} \"${config.services.jellyfin.dataDir}/root/default/${name}\""
        ) (builtins.attrNames jellyfin.mediaLibraries)
      )}
    '';
  };

  services.jellyseerr = {
    enable = true;
    openFirewall = true;
  };

  systemd.services.jellyseerr.serviceConfig = {
    CapabilityBoundingSet = "";
    ProcSubset = "pid";
    ProtectClock = true;
    ProtectProc = "invisible";
    RestrictNamespaces = true;
  };

  services.sonarr = {
    enable = true;
    package = pkgsUnstable.sonarr;
    dataDir = "/var/lib/sonarr";
  };

  users.users.sonarr.extraGroups = [ "qbittorrent" ];

  # Configure shows directory to be shared by sonarr group
  systemd.tmpfiles.settings.sonarr = builtins.listToAttrs (
    builtins.map (folder: {
      name = folder.path;
      value = {
        d = {
          mode = "2775";
          user = config.services.sonarr.user;
          group = config.services.sonarr.group;
        };

        a = {
          argument = "default:group::rwx";
        };
      };
    }) sonarr.rootFolders
  );

  systemd.services.sonarr = {
    preStart = ''
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${
        xmlFormat.generate "config.xml" {
          Config = {
            BindAddress = "*";
            AuthenticationMethod = "External";
            AnalyticsEnabled = false;
            LogDbEnabled = false;
            ApiKey = sonarr.apiKey;
          };
        }
      } "${config.services.sonarr.dataDir}/config.xml"
    '';

    postStart = ''
      ${makeCurlScript "sonarr-curl-script"
        ''
          silent
          show-error
          parallel
        ''
        ''
          header = "X-Api-Key: ${sonarr.apiKey}"
          header = "Content-Type: application/json"
          retry = 3
          retry-connrefused
        ''
        (
          let
            naming = ''
              fail-with-body
              url = "http://localhost:${sonarr.port}/api/v3/config/naming/1"
              request = "PUT"
              data = "@${
                jsonFormat.generate "naming.json" {
                  id = 1;
                  renameEpisodes = true;
                  replaceIllegalCharacters = true;
                  colonReplacementFormat = 0;
                  customColonReplacementFormat = "";
                  multiEpisodeStyle = 0;
                  standardEpisodeFormat = "{Series Title} - S{season:00}E{episode:00} - {Episode Title} {Quality Title} {MediaInfo VideoCodec}";
                  dailyEpisodeFormat = "{Series Title} - {Air-Date} - {Episode Title} {Quality Title} {MediaInfo VideoCodec}";
                  animeEpisodeFormat = "{Series Title} - S{season:00}E{episode:00} - {Episode Title} {Quality Title} {MediaInfo VideoCodec}";
                  seriesFolderFormat = "{Series Title}";
                  seasonFolderFormat = "Season {season}";
                  specialsFolderFormat = "Specials";
                }
              }"
            '';
          in
          [
            # PUT naming twice - first PUT doesn't work???
            naming
            naming
            ''
              fail-with-body
              url = "http://localhost:${sonarr.port}/api/v3/config/mediamanagement/1"
              request = "PUT"
              data = "@${
                jsonFormat.generate "mediamanagement.json" {
                  id = 1;
                  importExtraFiles = true;
                  extraFileExtensions = "srt";
                  deleteEmptyFolders = true;

                  # GUI defaults
                  copyUsingHardlinks = true;
                  recycleBinCleanupDays = 7;
                  minimumFreeSpaceWhenImporting = 100;
                  enableMediaInfo = true;
                }
              }"
            ''
          ]
          ++ (builtins.map (folder: ''
            url = "http://localhost:${sonarr.port}/api/v3/rootfolder"
            request = "POST"
            data = "@${jsonFormat.generate "rootfolder.json" folder}"
          '') sonarr.rootFolders)
          ++ (builtins.map (name: ''
            url = "http://localhost:${sonarr.port}/api/v3/downloadclient"
            request = "POST"
            data = "@${
              jsonFormat.generate "${name}.json" (
                {
                  inherit name;
                  enable = true;
                  removeCompletedDownloads = true;
                  removeFailedDownloads = true;
                }
                // makeArrConfig sonarr.downloadClients.${name}
              )
            }"
          '') (builtins.attrNames sonarr.downloadClients))
        )
      }
    '';
  };

  services.radarr = {
    enable = true;
    package = pkgsUnstable.radarr;
    dataDir = "/var/lib/radarr";
  };

  users.users.radarr.extraGroups = [ "qbittorrent" ];

  # Configure movies directory to be shared by radarr group
  systemd.tmpfiles.settings.radarr = builtins.listToAttrs (
    builtins.map (folder: {
      name = folder.path;
      value = {
        d = {
          mode = "2775";
          user = config.services.radarr.user;
          group = config.services.radarr.group;
        };

        a = {
          argument = "default:group::rwx";
        };
      };
    }) radarr.rootFolders
  );

  systemd.services.radarr = {
    preStart = ''
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${
        xmlFormat.generate "config.xml" {
          Config = {
            BindAddress = "*";
            AuthenticationMethod = "External";
            AnalyticsEnabled = false;
            LogDbEnabled = false;
            ApiKey = radarr.apiKey;
          };
        }
      } "${config.services.radarr.dataDir}/config.xml"
    '';

    postStart = ''
      ${makeCurlScript "radarr-curl-script"
        ''
          silent
          show-error
          parallel
        ''
        ''
          header = "X-Api-Key: ${radarr.apiKey}"
          header = "Content-Type: application/json"
          retry = 3
          retry-connrefused
        ''
        (
          let
            naming = ''
              fail-with-body
              url = "http://localhost:${radarr.port}/api/v3/config/naming/1"
              request = "PUT"
              data = "@${
                jsonFormat.generate "naming.json" {
                  renameMovies = true;
                  replaceIllegalCharacters = true;
                  standardMovieFormat = "{Movie Title} ({Release Year}) {Quality Title} {MediaInfo VideoCodec}";
                  movieFolderFormat = "{Movie Title} ({Release Year})";
                }
              }"
            '';
          in
          [
            # PUT naming twice - first PUT doesn't work???
            naming
            naming
            ''
              fail-with-body
              url = "http://localhost:${radarr.port}/api/v3/config/mediamanagement/1"
              request = "PUT"
              data = "@${
                jsonFormat.generate "mediamanagement.json" {
                  importExtraFiles = true;
                  extraFileExtensions = "srt";
                  deleteEmptyFolders = true;

                  # GUI defaults
                  copyUsingHardlinks = true;
                  recycleBinCleanupDays = 7;
                  minimumFreeSpaceWhenImporting = 100;
                  enableMediaInfo = true;
                }
              }"
            ''
          ]
          ++ (builtins.map (folder: ''
            url = "http://localhost:${radarr.port}/api/v3/rootfolder"
            request = "POST"
            data = "@${jsonFormat.generate "rootfolder.json" folder}"
          '') radarr.rootFolders)
          ++ (builtins.map (name: ''
            url = "http://localhost:${radarr.port}/api/v3/downloadclient"
            request = "POST"
            data = "@${
              jsonFormat.generate "${name}.json" (
                {
                  inherit name;
                  enable = true;
                  removeCompletedDownloads = true;
                  removeFailedDownloads = true;
                }
                // makeArrConfig radarr.downloadClients.${name}
              )
            }"
          '') (builtins.attrNames radarr.downloadClients))
        )
      }
    '';
  };

  services.prowlarr = {
    enable = true;
    package = pkgsUnstable.prowlarr;
  };

  systemd.services.prowlarr = {
    preStart = ''
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${
        xmlFormat.generate "config.xml" {
          Config = {
            BindAddress = "*";
            AuthenticationMethod = "External";
            AnalyticsEnabled = false;
            LogDbEnabled = false;
            ApiKey = prowlarr.apiKey;
          };
        }
      } "/var/lib/prowlarr/config.xml"
    '';

    postStart = ''
      ${makeCurlScript "prowlarr-curl-script"
        ''
          silent
          show-error
          parallel
        ''
        ''
          header = "X-Api-Key: ${prowlarr.apiKey}"
          header = "Content-Type: application/json"
          retry = 3
          retry-connrefused
        ''
        (
          (builtins.map (name: ''
            url = "http://localhost:${prowlarr.port}/api/v1/applications"
            request = "POST"
            data = "@${
              jsonFormat.generate "${name}.json" (
                {
                  inherit name;
                  enable = true;
                  appProfileId = 1;
                }
                // makeArrConfig prowlarr.applications.${name}
              )
            }"
          '') (builtins.attrNames prowlarr.applications))
          ++ (builtins.map (name: ''
            url = "http://localhost:${prowlarr.port}/api/v1/indexer"
            request = "POST"
            data = "@${
              jsonFormat.generate "${name}.json" (
                {
                  inherit name;
                  enable = true;
                  appProfileId = 1;
                  priority = 25;
                }
                // makeArrConfig prowlarr.indexers.${name}
              )
            }"
          '') (builtins.attrNames prowlarr.indexers))
          ++ (builtins.map (name: ''
            url = "http://localhost:${prowlarr.port}/api/v1/downloadclient"
            request = "POST"
            data = "@${
              jsonFormat.generate "${name}.json" (
                {
                  inherit name;
                  enable = true;
                  categories = [ ];
                }
                // makeArrConfig prowlarr.downloadClients.${name}
              )
            }"
          '') (builtins.attrNames prowlarr.downloadClients))
        )
      }
    '';
  };

  systemd.packages = [ pkgs.qbittorrent-nox ];

  users.users.qbittorrent = {
    uid = config.ids.uids.deluge;
    group = "qbittorrent";
    home = "/var/lib/qBittorrent";
    isSystemUser = true;
  };

  networking.firewall = {
    allowedTCPPorts = [ qBittorrent.BitTorrent."Session\\Port" ];
    allowedUDPPorts = [ qBittorrent.BitTorrent."Session\\Port" ];
  };

  users.groups.qbittorrent = {
    gid = config.ids.gids.deluge;
  };

  # Configure downloads directory to be shared by qbittorrent group
  systemd.tmpfiles.settings.qBittorrent.${qBittorrent.BitTorrent."Session\\DefaultSavePath"} = {
    d = {
      mode = "2755";
      user = "qbittorrent";
      group = "qbittorrent";
    };
    a = {
      argument = "default:group::rwx";
    };
  };

  systemd.services."qbittorrent-nox@qbittorrent" = {
    overrideStrategy = "asDropin";
    wantedBy = [ "multi-user.target" ];
    environment = {
      XDG_STATE_HOME = "/var/lib";
      XDG_DATA_HOME = "/var/lib";
      XDG_CACHE_HOME = "/var/cache";
      XDG_RUNTIME_DIR = "/run";
    };

    unitConfig = {
      RequiresMountsFor = [ qBittorrent.BitTorrent."Session\\DefaultSavePath" ];
    };

    serviceConfig = {
      StateDirectory = "qBittorrent";
      StateDirectoryMode = "0700";
      CacheDirectory = "qBittorrent";
      CacheDirectoryMode = "0700";
    };

    preStart = ''
      ${pkgs.coreutils}/bin/mkdir -p "$STATE_DIRECTORY/.config/qBittorrent"
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership \
        ${qbittorrentFormat.generate "qBittorrent.conf" qBittorrent} \
        "$STATE_DIRECTORY/.config/qBittorrent/qBittorrent.conf"
    '';
  };
}
