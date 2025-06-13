{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.syncthing;
  settings = cfg.settings;
  hostName = config.networking.hostName;
in
{
  services.syncthing = {
    enable = builtins.elem hostName (builtins.attrNames settings.devices);

    package = pkgs.syncthing.overrideAttrs (attrs: {
      patches = (attrs.patches or [ ]) ++ [ ./syncthing-dont-flatten-file-events.patch ];
    });

    user = "kira";
    group = "users";
    dataDir = "/home/kira";
    guiAddress = "0.0.0.0:8384";
    openDefaultPorts = true;
    overrideDevices = true;
    overrideFolders = true;

    settings = {
      devices = {
        luna.id = "O4NQTDT-NWV3GEZ-67BW33I-BQ454SI-42G2RK3-F53W4L4-RUG47VK-5VXLFA7";
      };

      folders = {
        Auth = {
          enable = builtins.elem hostName settings.folders.Auth.devices;
          devices = [ "luna" ];
          path = "~/Auth";
          caseSensitiveFS = true;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 1.0e-2;
          versioning = {
            type = "staggered";
            params.maxAge = "31536000";
          };
        };

        Dev = {
          enable = builtins.elem hostName settings.folders.Dev.devices;
          path = "~/Dev";
          caseSensitiveFS = true;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 1;
          versioning = {
            type = "staggered";
            params.maxAge = "604800";
          };
        };

        Documents = {
          enable = builtins.elem hostName settings.folders.Documents.devices;
          path = "~/Documents";
          caseSensitiveFS = true;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 1.0e-2;
          versioning = {
            type = "staggered";
            params.maxAge = "31536000";
          };
        };

        Pictures = {
          enable = builtins.elem hostName settings.folders.Pictures.devices;
          path = "~/Pictures";
          caseSensitiveFS = true;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 1.0e-2;
          versioning = {
            type = "staggered";
            params.maxAge = "31536000";
          };
        };

        Videos = {
          enable = builtins.elem hostName settings.folders.Videos.devices;
          path = "~/Videos";
          caseSensitiveFS = true;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 1.0e-2;
          versioning = {
            type = "staggered";
            params.maxAge = "31536000";
          };
        };
      };

      options = {
        autoUpgradeIntervalH = 0;
        crashReportingEnabled = false;
        maxConcurrentIncomingRequestKiB = 1024 * 1024;
        urAccepted = -1;
      };
    };
  };

  # Re-scanning on resume is very wasteful & power hungry
  systemd.services.syncthing-resume.enable = false;

  boot.kernel.sysctl = lib.mkIf cfg.enable {
    "fs.inotify.max_user_watches" = 1048576;
  };
}
