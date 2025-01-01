{
  config,
  lib,
  pkgsKiraNur,
  modulesPath,
  ...
}:

{
  imports = [
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")
  ];

  fileSystems = {
    "/" = lib.mkForce {
      device = "none";
      fsType = "tmpfs";
      options = [
        "defaults"
        "size=25%"
        "mode=755"
      ];
    };

    "/persist" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      neededForBoot = true;
      fsType = "ext4";
    };
  };

  environment.persistence."/persist".directories = [
    "/boot"
    "/home"
    "/nix"
  ];

  services.klipper = {
    enable = true;
    octoprintIntegration = true;

    firmwares = {
      mcu = {
        enable = true;
        configFile = ./ender3-v3-se-mcu.cfg;
        serial = "/dev/serial/by-id/usb-1a86_USB_Serial-if00-port0";
      };
    };

    configFile = "${pkgsKiraNur.ender3-v3-se-full-klipper}/printer.cfg";
  };

  services.octoprint.enable = true;

  services.nginx = {
    enable = true;
    virtualHosts = {
      "octoprint.jakira.space".locations."/" = {
        proxyPass = "http://${
          if config.services.octoprint.host != null then config.services.octoprint.host else "127.0.0.1"
        }:${toString config.services.octoprint.port}";

        recommendedProxySettings = true;
        proxyWebsockets = true;
      };
    };
  };
}
