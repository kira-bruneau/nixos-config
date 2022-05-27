{ lib, config, pkgs, ... }:

let
  appName = "Firefox";
  profile = "kira";
in
{
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-wayland;
    profiles = {
      ${profile} = {
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
          "browser.safebrowsing.malware.enabled" = false;
          "browser.safebrowsing.phishing.enabled" = false;
          "browser.search.suggest.enabled" = false;
          "browser.shell.checkDefaultBrowser" = false;
          "browser.startup.homepage" = "about:home";
          "browser.startup.page" = 3; # Restore previous session
          "browser.tabs.warnOnClose" = false;
          "browser.toolbars.bookmarks.visibility" = "newtab";
          "browser.uiCustomization.state" = builtins.toJSON {
            placements = {
              widget-overflow-fixed-list = [ ];
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
            currentVersion = 17;
            newElementCount = 2;
          };
          "browser.warnOnQuit" = false;
          "datareporting.healthreport.uploadEnabled" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;
          "devtools.selfxss.count" = 5; # Allow pasting into console
          "dom.security.https_only_mode" = true;
          "extensions.activeThemeID" = "firefox-alpenglow@mozilla.org";
          "extensions.formautofill.creditCards.available" = false;
          "extensions.formautofill.creditCards.enabled" = false;
          "layout.spellcheckDefault" = 1;
          "media.eme.enabled" = true;
          "services.sync.username" = "kira.bruneau@pm.me";
          "signon.rememberSignons" = false; # Use keepassxc instead
        };
      };
    };
  };

  xdg = {
    desktopEntries = {
      # Overrides upstream desktop entry to open firefox in a new tab
      firefox = {
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
    };

    mimeApps.defaultApplications = {
      "text/html" = "firefox.desktop";
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
    };
  };

  home = {
    file.".mozilla/firefox/${profile}/search.json.mozlz4" = {
      force = true;
      source = let
        current = "DuckDuckGo";

        # Firefox REALLY doesn't want to let me configure my search engine outside of Firefox... oh well, too bad!
        disclaimer =
          "By modifying this file, I agree that I am doing so " +
          "only within $appName itself, using official, user-driven search " +
          "engine selection processes, and in a way which does not circumvent " +
          "user consent. I acknowledge that any attempt to change this file " +
          "from outside of $appName is a malicious act, and will be responded " +
          "to accordingly.";

        # Hashing algorithm derived from ./toolkit/components/search/SearchUtils.jsm:getVerificationHash
        salt = profile + current + (builtins.replaceStrings [ "$appName" ] [ appName ] disclaimer);
        hash = builtins.readFile (pkgs.runCommandNoCC "${appName}-${profile}-${current}-hash" { inherit salt; } ''
          echo -n "$salt" | ${pkgs.openssl}/bin/openssl dgst -sha256 -binary | base64 | tr -d '\n' > "$out"
        '');
      in pkgs.runCommandNoCC "search.json.mozlz4" {} ''
        ${pkgs.mozlz4a}/bin/mozlz4a ${pkgs.writeText "search.json" (builtins.toJSON {
          version = 6;
          metaData = {
            current = current;
            hash = hash;
          };
        })} "$out"
      '';
    };

    sessionVariables = {
      # Touchscreen support
      MOZ_USE_XINPUT2 = "1";
    };
  };
}
