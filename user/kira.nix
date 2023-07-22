{ inputs, config, pkgs, ... }:

{
  imports = [
    inputs.home-manager.nixosModules.default
    ../group/audio.nix
  ];

  users.users.kira = {
    isNormalUser = true;
    description = "Kira Bruneau";
    extraGroups = [ "wheel" "adbusers" "audio" "video" "ipfs" ];
    initialPassword = "kira";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFolmVKlEFdALSIXtRNy/0ZqcTGn2H5/e3ieaIHoQr85"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMIUYCcFr41Oy49T6v4296m/5bD2w/HgIubL3rf+3ULW"
    ];
  };

  home-manager = {
    useUserPackages = true;
    users.kira = inputs.kira-home-config.nixosModules.${config.system.name};
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
        "atlantis" = { id = "64ZDVRR-2DZB475-3IWMGU6-OU46FZQ-P44AXVI-OYI6TO3-VOCUVRT-L62KBAE"; };
        "aurora" = { id = "ODCDVEV-I63ZAW6-MV27YBB-W5MDOAU-YZ3RK23-DMXCWAN-STJOSEF-EFXFRQP"; };
        "luna" = { id = "O4NQTDT-NWV3GEZ-67BW33I-BQ454SI-42G2RK3-F53W4L4-RUG47VK-5VXLFA7"; };
      };

      folders = {
        "Auth" = {
          enable = builtins.elem config.system.name [ "atlantis" "aurora" "luna" ];
          devices = [ "atlantis" "aurora" "luna" ];
          path = "~/Auth";
          ignorePerms = false;
          rescanIntervalS = 86400;
          versioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600";
              maxAge = "31536000";
            };
          };
        };

        # TODO: Automatically derive ignore rules from .gitignores
        # "Dev" = {
        #   enable = builtins.elem config.system.name [ "atlantis" "aurora" ];
        #   devices = [ "atlantis" "aurora" ];
        #   path = "~/Dev";
        #   ignorePerms = false;
        #   rescanIntervalS = 86400;
        #   versioning = {
        #     type = "simple";
        #     params.keep = "5";
        #   };
        # };

        "Documents" = {
          enable = builtins.elem config.system.name [ "atlantis" "aurora" ];
          devices = [ "atlantis" "aurora" ];
          path = "~/Documents";
          ignorePerms = false;
          rescanIntervalS = 86400;
          versioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600";
              maxAge = "31536000";
            };
          };
        };

        "Pictures" = {
          enable = builtins.elem config.system.name [ "atlantis" "aurora" ];
          devices = [ "atlantis" "aurora" ];
          path = "~/Pictures";
          ignorePerms = false;
          rescanIntervalS = 86400;
          versioning = {
            type = "simple";
            params.keep = "5";
          };
        };

        "Videos" = {
          enable = builtins.elem config.system.name [ "atlantis" "aurora" ];
          devices = [ "atlantis" "aurora" ];
          path = "~/Videos";
          ignorePerms = false;
          rescanIntervalS = 86400;
          versioning = {
            type = "simple";
            params.keep = "5";
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

  environment.etc."wpa_supplicant.conf" = {
    enable = config.networking.wireless.enable;
    source = pkgs.runCommandLocal "wpa_supplicant.conf" {} ''
      ln -s /home/kira/Auth/wpa_supplicant.conf "$out"
    '';
  };
}
