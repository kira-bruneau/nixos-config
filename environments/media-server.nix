{ config, lib, pkgs, ... }:

let
  downloadClients = {
    qBittorrent = {
      enable = true;
      implementation = "QBittorrent";
      configContract = "QBittorrentSettings";
      fields = [
        {
          name = "port";
          value = qBittorrent.Preferences."WebUI\\Port";
        }
      ];
    };
  };

  sonarr = {
    port = "8989";
    apiKey = "00000000000000000000000000000000";
    rootFolder = "/srv/media-ssd/shows";
    inherit downloadClients;
  };

  radarr = {
    port = "7878";
    apiKey = "00000000000000000000000000000000";
    rootFolder = "/srv/media-ssd/movies";
    inherit downloadClients;
  };

  prowlarr = {
    port = "9696";
    apiKey = "00000000000000000000000000000000";
    indexers = {
      "LimeTorrents" = {
        definitionFile = "limetorrents";
        downloadlink = 1; # iTorrents.org
        downloadlink2 = 0; # magnet
        sort = 0; # created
      };

      "Solid Torrents" = {
        definitionFile = "solidtorrents";
        sort = 0; # created
        type = 1; # desc
      };

      "The Pirate Bay" = {
        definitionFile = "thepiratebay";
      };

      "TheRARBG" = {
        definitionFile = "therarbg";
        sort = 0; # created desc
      };

      "YTS" = {
        definitionFile = "yts";
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
      "Session\\GlobalMaxRatio" = 2;
    };
  };

  jellyfin = {
    mediaLibraries = {
      Shows = {
        type = "tvshow";
        folders = [ sonarr.rootFolder "/srv/media-hdd/TV" ];
      };

      Movies = {
        type = "movies";
        folders = [ radarr.rootFolder "/srv/media-hdd/Movies" ];
      };

      Downloads.folders = [ qBittorrent.BitTorrent."Session\\DefaultSavePath" ];
    };
  };

  makeCurlScript = name: options: ctx: requests:
    pkgs.writeTextFile {
      inherit name;
      executable = true;
      text = ''
        #!${pkgs.curl}/bin/curl -K
        ${options}
        ${builtins.concatStringsSep "\nnext\n"
          (builtins.map
            (request: "${ctx}\n${request}")
            requests)}
      '';
    };

  makeMediaLibrary = name: { type ? null, folders }:
    pkgs.runCommand name { inherit type folders; } ''
      mkdir "$out"

      if [ -n "$type" ]; then
        touch "$out/$type.collection"
      fi

      for folder in ''${folders}; do
        echo -n "$folder" > "$out/$(basename "$folder").mblink"
      done
    '';
in
{
  services.jellyfin.enable = true;

  users.users.jellyfin.extraGroups = [ "sonarr" "radarr" "qbittorrent" ];

  systemd.services.jellyfin = {
    environment.JELLYFIN_LOG_DIR = "/var/log/jellyfin";
    serviceConfig = {
      LogsDirectory = "jellyfin";
      LogsDirectoryMode = "0700";
      UMask = lib.mkForce "0022";
      RequiresMountsFor = builtins.concatMap
        (lib: lib.folders)
        (builtins.attrValues jellyfin.mediaLibraries);
    };

    preStart = ''
      ${pkgs.coreutils}/bin/mkdir -p "$STATE_DIRECTORY/config"
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
      ''} "$STATE_DIRECTORY/config/system.xml"

      ${pkgs.coreutils}/bin/mkdir -p "$STATE_DIRECTORY/root/default"
      ${builtins.concatStringsSep "\n"
        (builtins.map
          (name: let lib = makeMediaLibrary name jellyfin.mediaLibraries.${name}; in
            "${pkgs.coreutils}/bin/ln -sfn ${lib} \"$STATE_DIRECTORY/root/default/${name}\"")
          (builtins.attrNames jellyfin.mediaLibraries))}
    '';
  };

  services.sonarr = {
    enable = true;
    dataDir = "/var/lib/sonarr";
  };

  users.users.sonarr.extraGroups = [ "qbittorrent" ];

  # Configure shows directory to be shared by sonarr group
  systemd.tmpfiles.settings.sonarr.${sonarr.rootFolder} = {
    d = {
      mode = "2770";
      user = config.services.sonarr.user;
      group = config.services.sonarr.group;
    };

    a = { argument = "default:group::rwx"; };
  };

  systemd.services.sonarr = {
    serviceConfig = {
      StateDirectory = "sonarr";
      StateDirectoryMode = "0700";
    };

    preStart = ''
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${pkgs.writeText "config.xml" ''
        <Config>
          <BindAddress>*</BindAddress>
          <AnalyticsEnabled>False</AnalyticsEnabled>
          <ApiKey>${sonarr.apiKey}</ApiKey>
        </Config>
      ''} "$STATE_DIRECTORY/config.xml"
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
          retry = 3
          retry-connrefused
        ''
        (let
          naming = ''
            fail-with-body
            url = "http://localhost:${sonarr.port}/api/v3/config/naming/1"
            request = "PUT"
            data = "@${pkgs.writers.writeJSON "naming.json" {
              id = 1;
              renameEpisodes = true;
              replaceIllegalCharacters = true;
              standardEpisodeFormat = "{Series Title} - S{season:00}E{episode:00} - {Episode Title} {Quality Title} {MediaInfo VideoCodec}";
              dailyEpisodeFormat = "{Series Title} - {Air-Date} - {Episode Title} {Quality Title} {MediaInfo VideoCodec}";
              animeEpisodeFormat = "{Series Title} - S{season:00}E{episode:00} - {Episode Title} {Quality Title} {MediaInfo VideoCodec}";
              seriesFolderFormat = "{Series Title}";
              seasonFolderFormat = "Season {season}";
              specialsFolderFormat = "Specials";
            }}"
          '';
        in
        [
          # PUT naming twice - first PUT doesn't work???
          naming naming
          ''
            fail-with-body
            url = "http://localhost:${sonarr.port}/api/v3/config/mediamanagement/1"
            request = "PUT"
            data = "@${pkgs.writers.writeJSON "mediamanagement.json" {
              id = 1;
              importExtraFiles = true;
              extraFileExtensions = "srt";

              # GUI defaults
              copyUsingHardlinks = true;
              recycleBinCleanupDays = 7;
              minimumFreeSpaceWhenImporting = 100;
              enableMediaInfo = true;
            }}"
          ''
          ''
            url = "http://localhost:${sonarr.port}/api/v3/rootfolder"
            request = "POST"
            data = "@${pkgs.writers.writeJSON "rootfolder.json" {
              path = sonarr.rootFolder;
            }}"
          ''
        ] ++ (builtins.map
          (name: ''
            url = "http://localhost:${sonarr.port}/api/v3/downloadclient"
            request = "POST"
            data = "@${pkgs.writers.writeJSON "${name}.json"
              ({
                inherit name;
                removeCompletedDownloads = true;
                removeFailedDownloads = true;
                fields = [];
              } // sonarr.downloadClients.${name}) }"
          '')
          (builtins.attrNames sonarr.downloadClients)))
      }
    '';
  };

  services.radarr = {
    enable = true;
    dataDir = "/var/lib/radarr";
  };

  users.users.radarr.extraGroups = [ "qbittorrent" ];

  # Configure movies directory to be shared by radarr group
  systemd.tmpfiles.settings.radarr.${radarr.rootFolder} = {
    d = {
      mode = "2770";
      user = config.services.radarr.user;
      group = config.services.radarr.group;
    };

    a = { argument = "default:group::rwx"; };
  };

  systemd.services.radarr = {
    serviceConfig = {
      StateDirectory = "radarr";
      StateDirectoryMode = "0700";
    };

    preStart = ''
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${pkgs.writeText "config.xml" ''
        <Config>
          <BindAddress>*</BindAddress>
          <AuthenticationMethod>External</AuthenticationMethod>
          <AnalyticsEnabled>False</AnalyticsEnabled>
          <ApiKey>${radarr.apiKey}</ApiKey>
        </Config>
      ''} "$STATE_DIRECTORY/config.xml"
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
        (let
          naming = ''
            fail-with-body
            url = "http://localhost:${radarr.port}/api/v3/config/naming/1"
            request = "PUT"
            data = "@${pkgs.writers.writeJSON "naming.json" {
              renameMovies = true;
              replaceIllegalCharacters = true;
              standardMovieFormat = "{Movie Title} ({Release Year}) {Quality Title} {MediaInfo VideoCodec}";
              movieFolderFormat = "{Movie Title} ({Release Year})";
            }}"
          '';
        in
        [
          # PUT naming twice - first PUT doesn't work???
          naming naming
          ''
            fail-with-body
            url = "http://localhost:${radarr.port}/api/v3/config/mediamanagement/1"
            request = "PUT"
            data = "@${pkgs.writers.writeJSON "mediamanagement.json" {
              importExtraFiles = true;
              extraFileExtensions = "srt";

              # GUI defaults
              copyUsingHardlinks = true;
              recycleBinCleanupDays = 7;
              minimumFreeSpaceWhenImporting = 100;
              enableMediaInfo = true;
            }}"
          ''
          ''
            url = "http://localhost:${radarr.port}/api/v3/rootfolder"
            request = "POST"
            data = "@${pkgs.writers.writeJSON "rootfolder.json" {
              path = radarr.rootFolder;
            }}"
          ''
        ] ++ (builtins.map
          (name: ''
            url = "http://localhost:${radarr.port}/api/v3/downloadclient"
            request = "POST"
            data = "@${pkgs.writers.writeJSON "${name}.json"
              ({
                inherit name;
                removeCompletedDownloads = true;
                removeFailedDownloads = true;
                fields = [];
              } // radarr.downloadClients.${name}) }"
          '')
          (builtins.attrNames radarr.downloadClients)))
      }
    '';
  };

  services.prowlarr.enable = true;

  systemd.services.prowlarr = {
    preStart = ''
      ${pkgs.coreutils}/bin/cp --no-preserve=mode,ownership ${pkgs.writeText "config.xml" ''
        <Config>
          <BindAddress>*</BindAddress>
          <AuthenticationMethod>External</AuthenticationMethod>
          <AnalyticsEnabled>False</AnalyticsEnabled>
          <ApiKey>${prowlarr.apiKey}</ApiKey>
        </Config>
      ''} "$STATE_DIRECTORY/config.xml"
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
        ([
          ''
            url = "http://localhost:${prowlarr.port}/api/v1/applications"
            request = "POST"
            data = "@${pkgs.writers.writeJSON "sonarr.json" {
              name = "Sonarr";
              syncLevel = "fullSync";
              implementation = "Sonarr";
              configContract = "SonarrSettings";
              fields = [
                {
                  name = "apiKey";
                  value = sonarr.apiKey;
                }
              ];
            }}"
          ''
          ''
            url = "http://localhost:${prowlarr.port}/api/v1/applications"
            request = "POST"
            data = "@${pkgs.writers.writeJSON "radarr.json" {
              name = "Radarr";
              syncLevel = "fullSync";
              implementation = "Radarr";
              configContract = "RadarrSettings";
              fields = [
                {
                  name = "apiKey";
                  value = radarr.apiKey;
                }
              ];
            }}"
          ''
        ]
        ++ (builtins.map
          (name: let fields = prowlarr.indexers.${name}; in ''
            url = "http://localhost:${prowlarr.port}/api/v1/indexer"
            request = "POST"
            data = "@${pkgs.writers.writeJSON "${name}.json" {
              inherit name;
              enable = true;
              appProfileId = 1;
              implementation = "Cardigann";
              configContract = "CardigannSettings";
              fields = builtins.map
                (name: {
                  inherit name;
                  value = fields.${name};
                })
                (builtins.attrNames fields);
            }}"
          '')
          (builtins.attrNames prowlarr.indexers))
        ++ (builtins.map
          (name: ''
            url = "http://localhost:${prowlarr.port}/api/v1/downloadclient"
            request = "POST"
            data = "@${pkgs.writers.writeJSON "${name}.json"
              ({
                inherit name;
                fields = [];
                categories = [];
              } // prowlarr.downloadClients.${name}) }"
          '')
          (builtins.attrNames prowlarr.downloadClients))
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
    d = { mode = "2770"; user = "qbittorrent"; group = "qbittorrent"; };
    a = { argument = "default:group::rwx"; };
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

  # Manage those jellies
  services.jellyseerr.enable = true;
}
