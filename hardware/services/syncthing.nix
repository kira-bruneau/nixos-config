{ config, pkgs, ... }:

{
  services.syncthing = {
    enable = builtins.elem config.system.name [
      "amethyst"
      "aurora"
      "luna"
      "quartz"
    ];
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
        "amethyst" = {
          id = "2PIQVSQ-2N77DGJ-XNTHNQF-PREKTRC-SCP6LFV-DRG3WK7-WFPT56T-NYWIAQG";
        };
        "aurora" = {
          id = "ODCDVEV-I63ZAW6-MV27YBB-W5MDOAU-YZ3RK23-DMXCWAN-STJOSEF-EFXFRQP";
        };
        "luna" = {
          id = "O4NQTDT-NWV3GEZ-67BW33I-BQ454SI-42G2RK3-F53W4L4-RUG47VK-5VXLFA7";
        };
        "quartz" = {
          id = "64ZDVRR-2DZB475-3IWMGU6-OU46FZQ-P44AXVI-OYI6TO3-VOCUVRT-L62KBAE";
        };
      };

      folders = {
        "Auth" = {
          enable = builtins.elem config.system.name [
            "amethyst"
            "aurora"
            "luna"
            "quartz"
          ];
          devices = [
            "amethyst"
            "aurora"
            "luna"
            "quartz"
          ];
          path = "~/Auth";
          caseSensitiveFS = true;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 1.0e-2;
          versioning = {
            type = "staggered";
            params.maxAge = "31536000";
          };
        };

        "Dev" = {
          enable = builtins.elem config.system.name [
            "amethyst"
            "aurora"
            "quartz"
          ];
          devices = [
            "amethyst"
            "aurora"
            "quartz"
          ];
          path = "~/Dev";
          caseSensitiveFS = true;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 1;
          versioning = {
            type = "staggered";
            params.maxAge = "604800";
          };
        };

        "Documents" = {
          enable = builtins.elem config.system.name [
            "amethyst"
            "aurora"
            "quartz"
          ];
          devices = [
            "amethyst"
            "aurora"
            "quartz"
          ];
          path = "~/Documents";
          caseSensitiveFS = true;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 1.0e-2;
          versioning = {
            type = "staggered";
            params.maxAge = "31536000";
          };
        };

        "Pictures" = {
          enable = builtins.elem config.system.name [
            "amethyst"
            "aurora"
            "quartz"
          ];
          devices = [
            "amethyst"
            "aurora"
            "quartz"
          ];
          path = "~/Pictures";
          caseSensitiveFS = true;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 1.0e-2;
          versioning = {
            type = "staggered";
            params.maxAge = "31536000";
          };
        };

        "Videos" = {
          enable = builtins.elem config.system.name [
            "amethyst"
            "aurora"
            "quartz"
          ];
          devices = [
            "amethyst"
            "aurora"
            "quartz"
          ];
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

      extraOptions = {
        options = {
          urAccepted = -1;
          maxConcurrentIncomingRequestKiB = 1024 * 1024;
        };
      };
    };
  };

  # Re-scanning on resume is very wasteful & power hungry
  systemd.services.syncthing-resume.enable = false;

  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches" = 1048576;
  };
}
