{ lib, config, pkgs, ... }:

{
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-wayland;
    profiles =
      let
        baseProfile = {
          search = {
            force = true;
            default = "DuckDuckGo";
            order = [ "DuckDuckGo" "Google" ];
            engines = {
              "Amazon.ca".metaData.alias = "@a";
              "Bing".metaData.hidden = true;
              "eBay".metaData.hidden = true;
              "Google".metaData.alias = "@g";
              "Wikipedia (en)".metaData.alias = "@w";

              "GitHub" = {
                urls = [{
                  template = "https://github.com/search";
                  params = [
                    { name = "q"; value = "{searchTerms}"; }
                  ];
                }];
                icon = "${pkgs.fetchurl {
                  url = "https://github.githubassets.com/favicons/favicon.svg";
                  sha256 = "sha256-apV3zU9/prdb3hAlr4W5ROndE4g3O1XMum6fgKwurmA=";
                }}";
                definedAliases = [ "@gh" ];
              };

              "Nix Packages" = {
                urls = [{
                  template = "https://search.nixos.org/packages";
                  params = [
                    { name = "channel"; value = "unstable"; }
                    { name = "query"; value = "{searchTerms}"; }
                  ];
                }];
                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@np" ];
              };

              "NixOS Wiki" = {
                urls = [{
                  template = "https://nixos.wiki/index.php";
                  params = [{ name = "search"; value = "{searchTerms}"; }];
                }];
                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@nw" ];
              };

              "Nixpkgs Issues" = {
                urls = [{
                  template = "https://github.com/NixOS/nixpkgs/issues";
                  params = [
                    { name = "q"; value = "{searchTerms}"; }
                  ];
                }];
                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@ni" ];
              };

              # A good way to find genuine discussion
              "Reddit" = {
                urls = [{
                  template = "https://www.reddit.com/search";
                  params = [
                    { name = "q"; value = "{searchTerms}"; }
                  ];
                }];
                icon = "${pkgs.fetchurl {
                  url = "https://www.redditstatic.com/accountmanager/favicon/favicon-512x512.png";
                  sha256 = "sha256-WiXqffmuCVCOJ/rpqyhFK59bz1lKnUOp9/aoEAYRsn0=";
                }}";
                definedAliases = [ "@r" ];
              };

              "Youtube" = {
                urls = [{
                  template = "https://www.youtube.com/results";
                  params = [{ name = "search_query"; value = "{searchTerms}"; }];
                }];
                icon = "${pkgs.fetchurl {
                  url = "www.youtube.com/s/desktop/8498231a/img/favicon_144x144.png";
                  sha256 = "sha256-lQ5gbLyoWCH7cgoYcy+WlFDjHGbxwB8Xz0G7AZnr9vI=";
                }}";
                definedAliases = [ "@y" ];
              };
            };
          };

          settings = {
            "accessibility.typeaheadfind.flashBar" = 0;
            "app.shield.optoutstudies.enabled" = false;
            "apz.overscroll.enabled" = true;
            "browser.aboutConfig.showWarning" = false;
            "browser.aboutwelcome.enabled" = false;
            "browser.contentblocking.category" = "strict";
            "browser.discovery.enabled" = false;
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
            "browser.newtabpage.activity-stream.feeds.section.highlights" = false;
            "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
            "browser.newtabpage.activity-stream.feeds.snippets" = false;
            "browser.newtabpage.activity-stream.feeds.telemetry" = false;
            "browser.newtabpage.activity-stream.feeds.topsites" = true;
            "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePinned" = "google"; # Don't autopin google on first run
            "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
            "browser.newtabpage.activity-stream.showSponsored" = false;
            "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
            "browser.newtabpage.activity-stream.telemetry" = false;
            "browser.newtabpage.blocked" = builtins.toJSON {
              # Dismiss builtin shortcuts
              "26UbzFJ7qT9/4DhodHKA1Q==" = 1;
              "4gPpjkxgZzXPVtuEoAL9Ig==" = 1;
              "eV8/WsSLxHadrTL1gAxhug==" = 1;
              "gLv0ja2RYVgxKdp0I5qwvA==" = 1;
              "oYry01JR5qiqP3ru9Hdmtg==" = 1;
              "T9nJot5PurhJSy8n038xGA==" = 1;
            };
            "browser.newtabpage.enabled" = true;
            "browser.newtabpage.pinned" = builtins.toJSON [ ];
            "browser.places.importBookmarksHTML" = true;
            "browser.safebrowsing.malware.enabled" = false;
            "browser.safebrowsing.phishing.enabled" = false;
            "browser.search.suggest.enabled" = false;
            "browser.search.update" = false;
            "browser.shell.checkDefaultBrowser" = false;
            "browser.startup.homepage" = "about:home";
            "browser.startup.page" = 3; # Restore previous session
            "browser.tabs.firefox-view" = false;
            "browser.tabs.warnOnClose" = false;
            "browser.toolbars.bookmarks.visibility" = "newtab";
            "browser.uiCustomization.state" = builtins.toJSON {
              placements = {
                widget-overflow-fixed-list = [ ];
                unified-extensions-area = [ ];
                nav-bar = [
                  "back-button"
                  "forward-button"
                  "stop-reload-button"
                  "urlbar-container"
                  "downloads-button"
                  "fxa-toolbar-menu-button"

                  # Extensions
                  "_testpilot-containers-browser-action"
                  "keepassxc-browser_keepassxc_org-browser-action"
                  "ublock0_raymondhill_net-browser-action"
                ];
                toolbar-menubar = [ "menubar-items" ];
                TabsToolbar = [
                  "tabbrowser-tabs"
                  "new-tab-button"
                  "alltabs-button"
                ];
                PersonalToolbar = [
                  "import-button"
                  "personal-bookmarks"
                ];
              };
              seen = [
                "save-to-pocket-button"
                "developer-button"

                # Extensions
                "_3385c2d8-dcfd-4f92-adb7-5d8429dee164_-browser-action" # Video Ad-Block, for Twitch
                "_9a41dee2-b924-4161-a971-7fb35c053a4a_-browser-action" # enhanced-h264ify
                "_d7742d87-e61d-4b78-b8a1-b469842139fa_-browser-action" # Vimium
                "_testpilot-containers-browser-action"
                "ghosttext_bfred_it-browser-action"
                "jid1-zadieub7xozojw_jetpack-browser-action" # Media Keys
                "keepassxc-browser_keepassxc_org-browser-action"
                "languagetool-webextension_languagetool_org-browser-action"
                "popupwindow_ettoolong-browser-action"
                "ublock0_raymondhill_net-browser-action"
                "_762f9885-5a13-4abd-9c77-433dcd38b8fd_-browser-action" # Return YouTube Dislike
              ];
              dirtyAreaCache = [
                "nav-bar"
                "toolbar-menubar"
                "TabsToolbar"
                "PersonalToolbar"
              ];
              currentVersion = 18;
              newElementCount = 2;
            };
            "browser.warnOnQuit" = false;
            "datareporting.healthreport.uploadEnabled" = false;
            "datareporting.policy.dataSubmissionEnabled" = false;
            "devtools.selfxss.count" = 5; # Allow pasting into console
            "dom.security.https_only_mode" = true;
            "extensions.formautofill.creditCards.available" = false;
            "extensions.formautofill.creditCards.enabled" = false;
            "layout.spellcheckDefault" = 1;
            "media.eme.enabled" = true;
            "services.sync.engine.creditcards" = false;
            "services.sync.engine.passwords" = false;
            "services.sync.engine.prefs" = false;
            "signon.rememberSignons" = false; # Use keepassxc instead
            "toolkit.telemetry.pioneer-new-studies-available" = false;
          };
        };
      in
      {
        ${config.home.username} = lib.mkMerge [
          baseProfile
          {
            id = 0;
            settings = {
              "extensions.activeThemeID" = "firefox-alpenglow@mozilla.org";
              "services.sync.username" = "kira.bruneau@pm.me";
            };
          }
        ];
        work = lib.mkMerge [
          baseProfile
          {
            id = 1;
            settings = {
              "extensions.activeThemeID" = "{d26a3404-d978-4bd6-93cf-f9749f57b923}";
              "services.sync.username" = "kira@gadget.dev";
            };
          }
        ];
      };
  };

  xdg = {
    desktopEntries =
      let
        # Overrides upstream desktop entry to open firefox in a new tab
        base = {
          categories = [ "Network" "WebBrowser" ];
          exec = "firefox --new-tab %U";
          genericName = "Web Browser";
          icon = "firefox";
          mimeType = [
            "application/vnd.mozilla.xul+xml"
            "application/xhtml+xml"
            "text/html"
            "text/xml"
            "x-scheme-handler/ftp"
            "x-scheme-handler/http"
            "x-scheme-handler/https"
          ];
          name = "Firefox";
          type = "Application";
        };
      in
      {
        firefox = base;
        firefox-work = lib.mkMerge [
          base
          {
            exec = lib.mkForce "${base.exec} -P work";
            name = lib.mkForce "${base.name} (work)";
          }
        ];
      };

    mimeApps.defaultApplications = {
      "application/vnd.mozilla.xul+xml" = "firefox.desktop";
      "application/xhtml+xml" = "firefox.desktop";
      "text/html" = "firefox.desktop";
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
    };
  };

  home = {
    activation.firefoxPermissions =
      let
        permissions = {
          "https://app.element.io" = {
            "desktop-notification" = "allow";
            "persistent-storage" = "allow";
          };
          "https://calendar.proton.me" = {
            "desktop-notification" = "allow";
          };
          "https://discord.com" = {
            "desktop-notification" = "allow";
          };
          "https://mail.proton.me" = {
            "desktop-notification" = "allow";
          };
        };

        db = lib.escapeShellArg "${config.home.homeDirectory}/.mozilla/firefox/${config.home.username}/permissions.sqlite";

        schemaSQL = pkgs.writeText "schema.sql" ''
          BEGIN TRANSACTION;

          PRAGMA user_version = 12;

          CREATE TABLE moz_perms(
            id INTEGER,
            origin TEXT,
            type TEXT,
            permission INTEGER,
            expireType INTEGER,
            expireTime INTEGER,
            modificationTime INTEGER,
            PRIMARY KEY(id)
          );

          -- Deprecated table, for backwards compatibility
          CREATE TABLE moz_hosts(
            id INTEGER,
            host TEXT,
            type TEXT,
            permission INTEGER,
            expireType INTEGER,
            expireTime INTEGER,
            modificationTime INTEGER,
            isInBrowserElement INTEGER,
            PRIMARY KEY(id)
          );

          COMMIT;
        '';

        sqlString = str: "'${builtins.replaceStrings [ "'" ] [ "''" ] str}'";
        sqlInt = int: toString int;

        permissionValue = {
          allow = 1;
          deny = 2;
          prompt = 3;
        };

        dataSQL = pkgs.writeText "data.sql" ''
          BEGIN TRANSACTION;

          -- Add a unique index on moz_perms(origin, type), to support upserts
          CREATE UNIQUE INDEX IF NOT EXISTS moz_perms_upsert_index ON moz_perms(origin, type);

          WITH now(unix_ms) AS (SELECT CAST((julianday('now') - 2440587.5) * 86400000 AS INTEGER))
          INSERT INTO moz_perms(origin, type, permission, expireType, expireTime, modificationTime)
          VALUES
          ${lib.concatStringsSep ",\n"
            (builtins.concatMap
              (origin:
                let
                  originPermissions = permissions.${origin};
                in
                (builtins.map
                  (type:
                    let
                      permission = permissionValue.${originPermissions.${type}};
                    in
                      "  (${sqlString origin}, ${sqlString type}, ${sqlInt permission}, 0, 0, (SELECT unix_ms FROM now))")
                  (builtins.attrNames originPermissions)))
              (builtins.attrNames permissions))}
          ON CONFLICT(origin, type) DO UPDATE SET
            permission=excluded.permission,
            expireType=excluded.expireType,
            expireTime=excluded.expireTime,
            modificationTime=excluded.modificationTime;

          COMMIT;
        '';
      in
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -e ${db} ]; then
          ${pkgs.sqlite}/bin/sqlite3 ${db} < ${schemaSQL}
        fi

        # Ignore errors, firefox enforces an exclusive lock on the db while running
        ${pkgs.sqlite}/bin/sqlite3 ${db} < ${dataSQL} || :
      '';

    # Touchscreen support
    sessionVariables.MOZ_USE_XINPUT2 = "1";
  };
}
