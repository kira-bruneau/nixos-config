{ config, lib, ... }:

{
  imports = [ ../groups/audio.nix ];

  users.users.kira = {
    isNormalUser = true;
    description = "Kira Bruneau";
    extraGroups = [
      "audio" # set higher memlock limit for yabridge
      "dialout" # access to serial devices (eg. CEC)
      "wheel" # admin privileges
    ]
    ++ lib.optional config.networking.networkmanager.enable "networkmanager"
    ++ lib.optional config.programs.adb.enable "adbusers"
    ++ lib.optional config.programs.gamemode.enable "gamemode"
    ++ lib.optional config.virtualisation.docker.enable "docker"
    ++ lib.optional config.services.kubo.enable "ipfs";

    hashedPasswordFile = "/persist/var/lib/secrets/login/kira";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFolmVKlEFdALSIXtRNy/0ZqcTGn2H5/e3ieaIHoQr85"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMIUYCcFr41Oy49T6v4296m/5bD2w/HgIubL3rf+3ULW"
    ];

    linger = true;
  };

  home-manager.users.kira = {
    imports = [ ../home/hosts/${config.system.name}.nix ];

    programs = {
      librewolf.profiles.librewolf.settings = {
        "extensions.activeThemeID" = "firefox-alpenglow@mozilla.org";
        "services.sync.username" = "kira.bruneau@pm.me";
      };

      git = {
        userName = "Kira Bruneau";
        userEmail = "kira.bruneau@pm.me";
        extraConfig = {
          gitlab.user = "kira-bruneau";
          github.user = "kira-bruneau";
        };
      };

      ssh.matchBlocks = {
        "peridot".user = "jakira";
        "peridot.lan".user = "jakira";
      };
    };
  };
}
