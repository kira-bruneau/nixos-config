{ config, lib, ... }:

{
  imports = [ ../groups/audio.nix ];

  users.users.kira = {
    isNormalUser = true;
    description = "Kira Bruneau";
    extraGroups =
      [
        "audio" # set higher memlock limit for yabridge
        "dialout" # access to serial devices (eg. CEC)
        "wheel" # admin privileges
      ]
      ++ lib.optional config.networking.networkmanager.enable "networkmanager"
      ++ lib.optional config.programs.adb.enable "adbusers"
      ++ lib.optional config.programs.gamemode.enable "gamemode"
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

    accounts.email = {
      order = [
        "kira.bruneau@pm.me"
        "kira.bruneau@gmail.com"
      ];

      accounts = {
        "kira.bruneau@pm.me" = {
          primary = true;
          address = "kira.bruneau@pm.me";
          aliases = [
            "kira.bruneau@protonmail.com"
            "kira.bruneau@proton.me"
          ];
          realName = "Kira Bruneau";
          userName = "kira.bruneau@pm.me";
          imap = {
            host = "127.0.0.1";
            port = 1143;
            tls.useStartTls = true;
          };
          smtp = {
            host = "127.0.0.1";
            port = 1025;
            tls.useStartTls = true;
          };
          thunderbird = {
            enable = true;
            profiles = [ "kira" ];
            settings = id: {
              # Use maildir instead of mbox
              "mail.server.server_${id}.storeContractID" = "@mozilla.org/msgstore/maildirstore;1";
            };
          };
        };

        "kira.bruneau@gmail.com" = {
          flavor = "gmail.com";
          address = "kira.bruneau@gmail.com";
          realName = "Kira Bruneau";
          smtp = lib.mkForce null; # Google sends local ip in SMTP headers
          thunderbird = {
            enable = true;
            profiles = [ "kira" ];
            settings = id: {
              # Use OAuth2 for authentication
              "mail.server.server_${id}.authMethod" = 10;

              # Disable notifications
              "mail.server.server_${id}.use_idle" = false;

              # Use maildir instead of mbox
              "mail.server.server_${id}.storeContractID" = "@mozilla.org/msgstore/maildirstore;1";
            };
          };
        };
      };
    };

    programs = {
      firefox.profiles.firefox.settings = {
        "extensions.activeThemeID" = "firefox-alpenglow@mozilla.org";
        "services.sync.username" = "kira.bruneau@pm.me";
      };

      thunderbird.profiles.kira.settings = {
        "extensions.activeThemeID" = "{14a690a4-9282-43f1-bb5e-81641b334ec2}";
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
        "amethyst".user = "kira";
        "amethyst.lan".user = "kira";
        "aurora".user = "kira";
        "aurora.lan".user = "kira";
        "jackflix".user = "jakira";
        "jackflix.lan".user = "jakira";
        "jakira.space".user = "kira";
        "quartz".user = "kira";
        "quartz.lan".user = "kira";
        "steamdeck".user = "jakira";
        "steamdeck.lan".user = "jakira";
      };
    };
  };
}
