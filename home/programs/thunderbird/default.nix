{
  lib,
  config,
  pkgs,
  pkgsUnstable,
  ...
}:

{
  imports = [ ./order.nix ];

  programs.thunderbird = {
    enable = true;

    package = pkgsUnstable.thunderbird-128.override {
      extraPolicies = {
        ExtensionSettings = {
          "uBlock0@raymondhill.net" = {
            installation_mode = "force_installed";
            install_url = "https://addons.thunderbird.net/thunderbird/downloads/latest/ublock-origin/latest.xpi";
          };

          "{67da5716-2a2f-4f39-b785-3ad9b7beeb6d}" = {
            installation_mode = "force_installed";
            install_url = "https://addons.thunderbird.net/thunderbird/downloads/latest/green-floral/latest.xpi";
          };
        };
      };
    };

    profiles = {
      thunderbird = {
        isDefault = true;
        settings = {
          # General
          "app.donation.eoy.version.viewed" = 1000;
          "mail.rights.version" = 1;
          "mail.shell.checkDefaultClient" = false;
          "mailnews.start_page.enabled" = false;

          # Phishing detection
          "mail.phishing.detection.enabled" = false;

          # Telemetry
          "datareporting.healthreport.uploadEnabled" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;

          # UI
          "mail.uidensity" = 2; # Relaxed
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

          # Workaround new local folders account being added on every launch
          # https://github.com/nix-community/home-manager/issues/5031
          "mail.accountmanager.accounts" = lib.concatStringsSep "," (
            (builtins.map (
              a: "account_${builtins.hashString "sha256" config.accounts.email.accounts.${a}.name}"
            ) (config.accounts.email.order))
            ++ [
              # RSS
              "account_rss"

              # Local Folder
              "account1"
            ]
          );

          # RSS account
          # TODO: Use home-manager module once merged:
          # https://github.com/nix-community/home-manager/pull/5613
          "mail.account.account_rss.server" = "server_rss";
          "mail.server.server_rss.name" = "RSS";
          "mail.server.server_rss.type" = "rss";
          "mail.server.server_rss.directory" = "${config.home.homeDirectory}/.thunderbird/thunderbird/Mail/Feeds";
          "mail.server.server_rss.directory-rel" = "[ProfD]Mail/Feeds";
          "mail.server.server_rss.hostname" = "Feeds";
          "mail.server.server_rss.storeContractID" = "@mozilla.org/msgstore/maildirstore;1";
        };

        userChrome =
          builtins.replaceStrings [ "https://upload.wikimedia.org/wikipedia/commons/0/05/UBlock_Origin.svg" ]
            [
              "file://${
                pkgs.fetchurl {
                  url = "https://upload.wikimedia.org/wikipedia/commons/0/05/UBlock_Origin.svg";
                  hash = "sha256-q+dlbA3VjulWXp3v8LBNIRvThMUHd+xaVbTr0sqJtZo=";
                }
              }"
            ]
            (builtins.readFile ./userChrome.css);

        userContent = builtins.readFile ./userContent.css;
      };
    };
  };

  wayland.windowManager.sway.config = {
    startup = [ { command = lib.getExe config.programs.thunderbird.package; } ];
    assigns."9" = [ { app_id = "^thunderbird$"; } ];
  };

  xdg.mimeApps.defaultApplications = {
    "message/rfc822" = "thunderbird.desktop";
    "x-scheme-handler/mailto" = "thunderbird.desktop";
    "x-scheme-handler/mid" = "thunderbird.desktop";
  };
}
