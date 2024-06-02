{ config, lib, ... }:

let
  system-name = config.system.name;
in
{
  users.users.jakira = {
    isNormalUser = true;
    description = "Jakira";
    extraGroups =
      [
        "wheel" # admin privileges
      ]
      ++ lib.optional config.networking.networkmanager.enable "networkmanager"
      ++ lib.optional config.programs.adb.enable "adbusers"
      ++ lib.optional config.services.kubo.enable "ipfs";

    hashedPasswordFile = "/persist/var/lib/secrets/login/jakira";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFolmVKlEFdALSIXtRNy/0ZqcTGn2H5/e3ieaIHoQr85"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMIUYCcFr41Oy49T6v4296m/5bD2w/HgIubL3rf+3ULW"
    ];

    linger = true;
  };

  home-manager.users.jakira =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      imports = [ ../home/hosts/${system-name}.nix ];

      # Add firefox profile for Jack
      programs.firefox.profiles = {
        firefox = lib.mkForce {
          name = "Kirafox";
          path = "kira";
        };
        jackfox =
          let
            base = config.programs.firefox.profiles.firefox;
          in
          {
            id = base.id + 1;
            name = "Jackfox";
            path = "jack";
            settings = base.settings // {
              "extensions.activeThemeID" = "{d26a3404-d978-4bd6-93cf-f9749f57b923}";
              "services.sync.username" = "jack.loder@outlook.com";
            };
          };
      };

      xdg.desktopEntries = {
        jackfox.icon = lib.mkForce (
          pkgs.fetchurl {
            url = "https://upload.wikimedia.org/wikipedia/commons/3/30/Firefox_Developer_Edition_logo%2C_2019.svg";
            hash = "sha256-gQk9Uz20oMJiA77HmlLp75VuwDudL64x7IPaz+PBca4=";
          }
        );
      };
    };
}
