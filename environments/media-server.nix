{
  config,
  lib,
  pkgs,
  pkgsUnstable,
  ...
}:

let
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
      { path = "/srv/media-hdd/shows"; }
      { path = "/srv/media-ssd/shows"; }
    ];

    inherit downloadClients;
  };

  radarr = {
    port = "7878";
    apiKey = "00000000000000000000000000000000";
    rootFolders = [
      { path = "/srv/media-hdd/movies"; }
      { path = "/srv/media-ssd/movies"; }
    ];

    inherit downloadClients;
  };

  readarr = {
    port = "8787";
    apiKey = "00000000000000000000000000000000";
    rootFolders = [
      {
        name = "Books";
        path = "/srv/media-hdd/books";
      }
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
      "Readarr" = {
        syncLevel = "fullSync";
        implementation = "Readarr";
        fields.apiKey = readarr.apiKey;
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
      "Session\\DefaultSavePath" = "/srv/media-hdd/downloads";
      "Session\\Port" = 15982;
      "Session\\GlobalMaxRatio" = 2;
    };
  };

  jellyfin = {
    mediaLibraries = {
      Shows = {
        type = "tvshows";
        folders = sonarr.rootFolders;
      };

      Movies = {
        type = "movies";
        folders = radarr.rootFolders;
      };

      Books = {
        type = "books";
        folders = readarr.rootFolders;
      };

      Downloads = {
        folders = [ { path = qBittorrent.BitTorrent."Session\\DefaultSavePath"; } ];
        options = ''
          <?xml version="1.0" encoding="utf-8"?>
          <LibraryOptions xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
            <EnableEmbeddedTitles>true</EnableEmbeddedTitles>
          </LibraryOptions>
        '';
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
        inherit type options;
        folders = builtins.map (folder: folder.path) folders;
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
          if [ -n "$options" ]; then
            echo -n "$options" > "$out/options.xml"
          fi
        done
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
  services.homepage-dashboard = {
    enable = true;
    listenPort = 80;
    services = [
      {
        "Public" = [
          {
            "Jellyfin" = {
              icon = "jellyfin.svg";
              href = "http://quartz:8096";
            };
          }
          {
            "Jellyseerr" = {
              icon = "jellyseerr.svg";
              href = "http://quartz:5055";
            };
          }
          {
            "BigChadGuys Plus" = {
              icon = "https://media.forgecdn.net/avatars/937/629/638416082100123870.png";
              widget = {
                type = "minecraft";
                url = "udp://amethyst:25565";
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
              href = "http://quartz:${sonarr.port}";
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
              href = "http://quartz:${radarr.port}";
              widget = {
                type = "radarr";
                url = "http://localhost:${radarr.port}";
                key = radarr.apiKey;
                # enableQueue = true;
              };
            };
          }
          {
            "Readarr" = {
              icon = "readarr.svg";
              href = "http://quartz:${readarr.port}";
              widget = {
                type = "readarr";
                url = "http://localhost:${readarr.port}";
                key = readarr.apiKey;
                # enableQueue = true;
              };
            };
          }
          {
            "Prowlarr" = {
              icon = "prowlarr.svg";
              href = "http://quartz:${prowlarr.port}";
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
              href = "http://quartz:${toString qBittorrent.Preferences."WebUI\\Port"}";
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
            "/srv/media-hdd"
          ];
        };
      }
    ];
  };

  systemd.services.homepage-dashboard.serviceConfig = {
    AmbientCapabilities = "CAP_NET_BIND_SERVICE";
    CapabilityBoundingSet = "CAP_NET_BIND_SERVICE";
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
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${pkgs.writeText "system.xml" ''
        <?xml version="1.0" encoding="utf-8"?>
        <ServerConfiguration xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
          <IsStartupWizardCompleted>true</IsStartupWizardCompleted>

          <UICulture>en-US</UICulture>
          <PreferredMetadataLanguage>en</PreferredMetadataLanguage>
          <MetadataCountryCode>CA</MetadataCountryCode>

          <PluginRepositories>
            <RepositoryInfo>
              <Name>Jellyfin Stable</Name>
              <Url>https://repo.jellyfin.org/releases/plugin/manifest-stable.json</Url>
              <Enabled>true</Enabled>
            </RepositoryInfo>
          </PluginRepositories>

          <LibraryMonitorDelay>1</LibraryMonitorDelay>
        </ServerConfiguration>
      ''} "${config.services.jellyfin.configDir}/system.xml"

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
          mode = "2770";
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
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${pkgs.writeText "config.xml" ''
        <Config>
          <BindAddress>*</BindAddress>
          <AuthenticationMethod>External</AuthenticationMethod>
          <AnalyticsEnabled>False</AnalyticsEnabled>
          <LogDbEnabled>False</LogDbEnabled>
          <ApiKey>${sonarr.apiKey}</ApiKey>
        </Config>
      ''} "${config.services.sonarr.dataDir}/config.xml"
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
                pkgs.writers.writeJSON "naming.json" {
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
                pkgs.writers.writeJSON "mediamanagement.json" {
                  id = 1;
                  importExtraFiles = true;
                  extraFileExtensions = "srt";

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
            data = "@${pkgs.writers.writeJSON "rootfolder.json" folder}"
          '') sonarr.rootFolders)
          ++ (builtins.map (name: ''
            url = "http://localhost:${sonarr.port}/api/v3/downloadclient"
            request = "POST"
            data = "@${
              pkgs.writers.writeJSON "${name}.json" (
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
          mode = "2770";
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
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${pkgs.writeText "config.xml" ''
        <Config>
          <BindAddress>*</BindAddress>
          <AuthenticationMethod>External</AuthenticationMethod>
          <AnalyticsEnabled>False</AnalyticsEnabled>
          <LogDbEnabled>False</LogDbEnabled>
          <ApiKey>${radarr.apiKey}</ApiKey>
        </Config>
      ''} "${config.services.radarr.dataDir}/config.xml"
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
                pkgs.writers.writeJSON "naming.json" {
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
                pkgs.writers.writeJSON "mediamanagement.json" {
                  importExtraFiles = true;
                  extraFileExtensions = "srt";

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
            data = "@${pkgs.writers.writeJSON "rootfolder.json" folder}"
          '') radarr.rootFolders)
          ++ (builtins.map (name: ''
            url = "http://localhost:${radarr.port}/api/v3/downloadclient"
            request = "POST"
            data = "@${
              pkgs.writers.writeJSON "${name}.json" (
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

  services.readarr = {
    enable = true;
    package = pkgsUnstable.readarr;
  };

  users.users.readarr.extraGroups = [ "qbittorrent" ];

  # Configure books directory to be shared by readarr group
  systemd.tmpfiles.settings.readarr = builtins.listToAttrs (
    builtins.map (folder: {
      name = folder.path;
      value = {
        d = {
          mode = "2770";
          user = config.services.readarr.user;
          group = config.services.readarr.group;
        };

        a = {
          argument = "default:group::rwx";
        };
      };
    }) readarr.rootFolders
  );

  systemd.services.readarr = {
    preStart = ''
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${pkgs.writeText "config.xml" ''
        <Config>
          <BindAddress>*</BindAddress>
          <AuthenticationMethod>External</AuthenticationMethod>
          <AnalyticsEnabled>False</AnalyticsEnabled>
          <LogDbEnabled>False</LogDbEnabled>
          <ApiKey>${readarr.apiKey}</ApiKey>
        </Config>
      ''} "${config.services.readarr.dataDir}/config.xml"
    '';

    postStart = ''
      ${makeCurlScript "readarr-curl-script"
        ''
          silent
          show-error
          parallel
        ''
        ''
          header = "X-Api-Key: ${readarr.apiKey}"
          header = "Content-Type: application/json"
          retry = 3
          retry-connrefused
        ''
        (
          let
            naming = ''
              fail-with-body
              url = "http://localhost:${readarr.port}/api/v1/config/naming/1"
              request = "PUT"
              data = "@${
                pkgs.writers.writeJSON "naming.json" {
                  renameBooks = true;
                  replaceIllegalCharacters = true;
                  colonReplacementFormat = 4;
                  standardBookFormat = "{Book Title}/{Author Name} - {Book Title}{ (PartNumber)}";
                  authorFolderFormat = "{Author Name}";
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
              url = "http://localhost:${readarr.port}/api/v1/config/mediamanagement/1"
              request = "PUT"
              data = "@${
                pkgs.writers.writeJSON "mediamanagement.json" {
                  importExtraFiles = true;
                  extraFileExtensions = "srt";

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
            url = "http://localhost:${readarr.port}/api/v1/rootfolder"
            request = "POST"
            data = "@${
              pkgs.writers.writeJSON "rootfolder.json" (
                {
                  defaultQualityProfileId = 1;
                  defaultMetadataProfileId = 1;
                }
                // folder
              )
            }"
          '') readarr.rootFolders)
          ++ (builtins.map (name: ''
            url = "http://localhost:${readarr.port}/api/v1/downloadclient"
            request = "POST"
            data = "@${
              pkgs.writers.writeJSON "${name}.json" (
                {
                  inherit name;
                  enable = true;
                  removeCompletedDownloads = true;
                  removeFailedDownloads = true;
                }
                // makeArrConfig readarr.downloadClients.${name}
              )
            }"
          '') (builtins.attrNames readarr.downloadClients))
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
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${pkgs.writeText "config.xml" ''
        <Config>
          <BindAddress>*</BindAddress>
          <AuthenticationMethod>External</AuthenticationMethod>
          <AnalyticsEnabled>False</AnalyticsEnabled>
          <LogDbEnabled>False</LogDbEnabled>
          <ApiKey>${prowlarr.apiKey}</ApiKey>
        </Config>
      ''} "/var/lib/prowlarr/config.xml"
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
              pkgs.writers.writeJSON "${name}.json" (
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
              pkgs.writers.writeJSON "${name}.json" (
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
              pkgs.writers.writeJSON "${name}.json" (
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

  users.groups.qbittorrent = {
    gid = config.ids.gids.deluge;
  };

  # Configure downloads directory to be shared by qbittorrent group
  systemd.tmpfiles.settings.qBittorrent.${qBittorrent.BitTorrent."Session\\DefaultSavePath"} = {
    d = {
      mode = "2770";
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

    serviceConfig = {
      StateDirectory = "qBittorrent";
      StateDirectoryMode = "0700";
      CacheDirectory = "qBittorrent";
      CacheDirectoryMode = "0700";
    };

    preStart = ''
      ${pkgs.coreutils}/bin/mkdir -p "$STATE_DIRECTORY/.config/qBittorrent"
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership \
        ${(pkgs.formats.ini { }).generate "qBittorrent.conf" qBittorrent} \
        "$STATE_DIRECTORY/.config/qBittorrent/qBittorrent.conf"
    '';
  };
}
