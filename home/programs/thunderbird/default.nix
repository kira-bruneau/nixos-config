{
  lib,
  config,
  pkgs,
  ...
}:

{
  programs.thunderbird = {
    enable = true;

    package =
      (pkgs.thunderbird-latest.overrideAttrs (attrs: {
        buildCommand =
          attrs.buildCommand
          + ''
            wrapProgram $out/bin/thunderbird \
              --run '${lib.getExe pkgs.fd} -e msf . ${config.home.homeDirectory}/.thunderbird/${config.home.username}/Mail/Feeds -x ${pkgs.coreutils}/bin/rm'
          '';
      })).override
        {
          extraPolicies = {
            ExtensionSettings = {
              "filtaquilla@mesquilla.com.xpi" = {
                installation_mode = "force_installed";
                install_url = "https://addons.thunderbird.net/thunderbird/downloads/latest/filtaquilla/latest.xpi";
              };

              "gconversation@xulforum.org" = {
                installation_mode = "force_installed";
                install_url = "https://addons.thunderbird.net/thunderbird/downloads/latest/gmail-conversation-view/latest.xpi";
              };

              "quickFilters@axelg.com.xpi" = {
                installation_mode = "force_installed";
                install_url = "https://addons.thunderbird.net/thunderbird/downloads/latest/quickfilters/latest.xpi";
              };

              "uBlock0@raymondhill.net" = {
                installation_mode = "force_installed";
                install_url = "https://addons.thunderbird.net/thunderbird/downloads/latest/ublock-origin/latest.xpi";
              };

              "{14a690a4-9282-43f1-bb5e-81641b334ec2}" = {
                installation_mode = "force_installed";
                install_url = "https://addons.thunderbird.net/thunderbird/downloads/latest/sakura-blossoms-birds/latest.xpi";
              };

              "{67da5716-2a2f-4f39-b785-3ad9b7beeb6d}" = {
                installation_mode = "force_installed";
                install_url = "https://addons.thunderbird.net/thunderbird/downloads/latest/green-floral/latest.xpi";
              };

              "{a300a000-5e21-4ee0-a115-9ec8f4eaa92b}" = {
                installation_mode = "force_installed";
                install_url = "https://addons.thunderbird.net/thunderbird/downloads/latest/removedupes/latest.xpi";
              };
            };
          };
        };

    profiles = {
      ${config.home.username} = {
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
          "apz.gtk.pangesture.delta_mode" = 2; # pixel mode
          "apz.gtk.pangesture.pixel_delta_mode_multiplier" = "80"; # default is 40
          "apz.overscroll.enabled" = true; # NOTE: Doesn't apply to browser chrome like it does in firefox
          "mail.biff.play_sound" = false; # Disable notification sound
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
          "mail.server.server_rss.directory" = "${config.home.homeDirectory}/.thunderbird/${config.home.username}/Mail/Feeds";
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
    startup = [ { command = "thunderbird"; } ];
    assigns."9" = [ { app_id = "^thunderbird$"; } ];
  };

  xdg.mimeApps.defaultApplications = {
    "message/rfc822" = "thunderbird.desktop";
    "x-scheme-handler/mailto" = "thunderbird.desktop";
    "x-scheme-handler/mid" = "thunderbird.desktop";
  };

  home.file.".thunderbird/${config.home.username}/Mail/Feeds/.stignore".text = ''
    *.backup
    *.msf
  '';
}
