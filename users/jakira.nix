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
        "dialout" # access to serial devices (eg. CEC)
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

      # Add librewolf profile for Jack
      programs.librewolf.profiles = {
        librewolf = {
          name = lib.mkForce "Kirawolf";
          path = lib.mkForce "kira";
        };
        jackwolf =
          let
            base = config.programs.librewolf.profiles.librewolf;
          in
          {
            id = base.id + 1;
            name = "Jackwolf";
            path = "jack";
            settings = base.settings // {
              "extensions.activeThemeID" = "{d26a3404-d978-4bd6-93cf-f9749f57b923}";
              "services.sync.username" = "jack.loder@outlook.com";
            };
          };
      };

      xdg.desktopEntries = {
        librewolf.icon = lib.mkForce (
          pkgs.fetchurl {
            url = "https://raw.githubusercontent.com/chunkyhairball/cute-icons/refs/heads/main/librewoof/Librewoof.svg";
            hash = "sha256-V0yN2cMm07zNilfRWJdxASSxBOF2yNb9Bsd0kQi11Ms=";
          }
        );

        jackwolf.icon = lib.mkForce "librewolf";
      };
    };
}
