{ inputs, config, lib, ... }:

{
  imports = [
    inputs.home-manager.nixosModules.default
    ../groups/audio.nix
  ];

  users.users.kira = {
    isNormalUser = true;
    description = "Kira Bruneau";
    extraGroups =
      [
        "audio" # set higher memlock limit for yabridge
        "dialout" # access to serial devices (eg. CEC)
        "wheel" # admin privileges
      ]
      ++ lib.optional config.programs.adb.enable "adbusers"
      ++ lib.optional config.services.kubo.enable "ipfs";

    initialPassword = "kira";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFolmVKlEFdALSIXtRNy/0ZqcTGn2H5/e3ieaIHoQr85"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMIUYCcFr41Oy49T6v4296m/5bD2w/HgIubL3rf+3ULW"
    ];
  };

  home-manager = {
    useUserPackages = true;
    extraSpecialArgs = { inherit inputs; };
    users.kira = ../home/hosts/${config.system.name}.nix;
  };

  services.syncthing = {
    enable = true;
    user = "kira";
    group = "users";
    dataDir = "/home/kira";
    openDefaultPorts = true;
    overrideDevices = true;
    overrideFolders = true;
    settings = {
      devices = {
        "aurora" = { id = "ODCDVEV-I63ZAW6-MV27YBB-W5MDOAU-YZ3RK23-DMXCWAN-STJOSEF-EFXFRQP"; };
        "luna" = { id = "O4NQTDT-NWV3GEZ-67BW33I-BQ454SI-42G2RK3-F53W4L4-RUG47VK-5VXLFA7"; };
        "quartz" = { id = "64ZDVRR-2DZB475-3IWMGU6-OU46FZQ-P44AXVI-OYI6TO3-VOCUVRT-L62KBAE"; };
      };

      folders = {
        "Auth" = {
          enable = builtins.elem config.system.name [ "aurora" "luna" "quartz" ];
          devices = [ "aurora" "luna" "quartz" ];
          path = "~/Auth";
          ignorePerms = false;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 0.01;
          versioning = {
            type = "staggered";
            params.maxAge = "31536000";
          };
        };

        "Dev" = {
          enable = builtins.elem config.system.name [ "aurora" "quartz" ];
          devices = [ "aurora" "quartz" ];
          path = "~/Dev";
          ignorePerms = false;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 1;
          versioning = {
            type = "staggered";
            params.maxAge = "604800";
          };
        };

        "Documents" = {
          enable = builtins.elem config.system.name [ "aurora" "quartz" ];
          devices = [ "aurora" "quartz" ];
          path = "~/Documents";
          ignorePerms = false;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 0.01;
          versioning = {
            type = "staggered";
            params.maxAge = "31536000";
          };
        };

        "Pictures" = {
          enable = builtins.elem config.system.name [ "aurora" "quartz" ];
          devices = [ "aurora" "quartz" ];
          path = "~/Pictures";
          ignorePerms = false;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 0.01;
          versioning = {
            type = "staggered";
            params.maxAge = "31536000";
          };
        };

        "Videos" = {
          enable = builtins.elem config.system.name [ "aurora" "quartz" ];
          devices = [ "aurora" "quartz" ];
          path = "~/Videos";
          ignorePerms = false;
          rescanIntervalS = 86400;
          fsWatcherDelayS = 0.01;
          versioning = {
            type = "staggered";
            params.maxAge = "31536000";
          };
        };
      };

      extraOptions = {
        options = {
          urAccepted = -1;
        };
      };
    };
  };

  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches" = 1048576;
  };
}