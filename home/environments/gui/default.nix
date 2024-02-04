{ pkgs, ... }:

{
  imports = [
    ../../environments/default.nix
    ../../environments/wallpapers.nix

    # Web
    ../../programs/firefox
    ../../programs/jellyfin
    ../../programs/newsflash

    # Media & Documents
    ../../programs/emacs
    ../../programs/keepassxc

    # Utils
    ../../programs/speedcrunch
    ../../programs/gnome-pomodoro
  ];

  home = {
    packages = with pkgs; [
      # Web
      qbittorrent
      syncplay
      ungoogled-chromium
      yt-dlp

      # Chat
      discord

      # Fonts
      inter

      # Utils
      libnotify
      xdg-utils
    ];

    pointerCursor = {
      package = pkgs.gnome.adwaita-icon-theme;
      name = "Adwaita";
      size = 24;
    };

    sessionVariables = {
      # Use Wayland for Chrome & Electron apps
      NIXOS_OZONE_WL = 1;

      # Improve appearance of Java applications
      # https://wiki.archlinux.org/index.php/Java#Tips_and_tricks
      JDK_JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel";
    };
  };

  xdg = {
    mimeApps.enable = true;

    # mimeapps.list often gets overwritten by applications adding mimetype associations
    configFile."mimeapps.list".force = true;
  };

  fonts.fontconfig.enable = true;
  xdg.configFile."fontconfig/fonts.conf".text = ''
    <?xml version='1.0'?>
    <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
    <fontconfig>
      <alias binding="same">
      <family>sans-serif</family>
      <prefer><family>Inter</family></prefer>
      </alias>
    </fontconfig>
  '';

  xdg.configFile."wireplumber/main.lua.d/51-restrict-control.lua".text = ''
    table.insert(default_access.rules, {
      matches = {
        {
          { "application.process.binary", "matches", "*chromium*" },
        },
        {
          { "application.process.binary", "matches", "*Discord*" },
        },
        {
          { "application.process.binary", "matches", "*electron*" },
        },
        {
          { "application.process.binary", "matches", "*firefox*" },
        },
      },
      default_permissions = "rx",
    })
  '';

  services.easyeffects.enable = true;

  wayland.windowManager.sway.config = {
    assigns = {
      "1" = [
        { app_id = "^chromium-browser$"; }
      ];
      "4" = [
        { app_id = "^org.qbittorrent.qBittorrent$"; }
        { title = "^Syncplay"; }
      ];
      "10" = [
        { app_id = "^Caprine$"; }
        { app_id = "^discord$"; }
      ];
    };

    window.commands = [
      {
        criteria = { app_id = "^discord$"; title = "^$"; };
        command = "floating enable, sticky enable, border pixel 0, resize set 480 270, move position 1004 680, opacity 0.8";
      }
    ];
  };
}
