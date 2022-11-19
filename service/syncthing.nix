{ config, ... }:

{
  services.syncthing = {
    enable = true;
    user = "kira";
    group = "users";
    dataDir = "/home/kira";
    openDefaultPorts = true;

    overrideDevices = true;
    devices = {
      "atlantis" = { id = "64ZDVRR-2DZB475-3IWMGU6-OU46FZQ-P44AXVI-OYI6TO3-VOCUVRT-L62KBAE"; };
      "aurora" = { id = "ODCDVEV-I63ZAW6-MV27YBB-W5MDOAU-YZ3RK23-DMXCWAN-STJOSEF-EFXFRQP"; };
      "luna" = { id = "O4NQTDT-NWV3GEZ-67BW33I-BQ454SI-42G2RK3-F53W4L4-RUG47VK-5VXLFA7"; };
    };

    overrideFolders = true;
    folders = {
      "Auth" = {
        enable = builtins.elem config.system.name [ "atlantis" "aurora" "luna" ];
        devices = [ "atlantis" "aurora" "luna" ];
        path = "~/Auth";
        ignorePerms = false;
        rescanInterval = 86400;
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
      #   rescanInterval = 86400;
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
        rescanInterval = 86400;
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
        rescanInterval = 86400;
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
        rescanInterval = 86400;
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
}
