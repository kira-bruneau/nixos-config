{ pkgs, ... }:

{
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-wayland;
    profiles = {
      kira = {
        settings = {
          "app.shield.optoutstudies.enabled" = false;
          "browser.aboutConfig.showWarning" = false;
          "browser.aboutwelcome.enabled" = false;
          "browser.contentblocking.category" = "strict";
          "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
          "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
          "browser.newtabpage.activity-stream.feeds.section.highlights" = false;
          "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
          "browser.newtabpage.activity-stream.feeds.snippets" = false;
          "browser.newtabpage.activity-stream.feeds.telemetry" = false;
          "browser.newtabpage.activity-stream.feeds.topsites" = true;
          "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
          "browser.newtabpage.activity-stream.showSponsored" = false;
          "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
          "browser.newtabpage.activity-stream.telemetry" = false;
          "browser.newtabpage.enabled" = true;
          "browser.newtabpage.pinned" = builtins.toJSON [ ];
          "browser.safebrowsing.malware.enabled" = false;
          "browser.safebrowsing.phishing.enabled" = false;
          "browser.search.suggest.enabled" = false;
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
          "datareporting.healthreport.uploadEnabled" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;
          "devtools.selfxss.count" = 5; # Allow pasting into console
          "dom.security.https_only_mode" = true;
          "extensions.activeThemeID" = "firefox-alpenglow@mozilla.org";
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
        terminal = false;
        type = "Application";
      };
    };
  };

  home.sessionVariables = {
    # Touchscreen support on Firefox
    MOZ_USE_XINPUT2 = "1";
  };
}
