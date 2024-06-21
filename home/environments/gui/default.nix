{ pkgs, ... }:

{
  imports = [
    ../../environments/wallpapers.nix

    # Web
    ../../programs/firefox
    ../../programs/jellyfin
    ../../programs/newsflash

    # Chat
    ../../programs/element

    # Media & Documents
    ../../programs/emacs
    ../../programs/keepassxc

    # Utils
    ../../programs/gnome-pomodoro
    ../../programs/gpg
    ../../programs/speedcrunch
  ];

  home = {
    packages = with pkgs; [
      # Web
      qbittorrent
      syncplay
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

  fonts.fontconfig = {
    enable = true;
    defaultFonts.sansSerif = [ "Inter" ];
  };

  # xdg.configFile."wireplumber/main.lua.d/51-restrict-control.lua".text = ''
  #   table.insert(default_access.rules, {
  #     matches = {
  #       {
  #         { "application.process.binary", "matches", "*chromium*" },
  #       },
  #       {
  #         { "application.process.binary", "matches", "*Discord*" },
  #       },
  #       {
  #         { "application.process.binary", "matches", "*electron*" },
  #       },
  #       {
  #         { "application.process.binary", "matches", "*firefox*" },
  #       },
  #     },
  #     default_permissions = "rx",
  #   })
  # '';

  wayland.windowManager.sway.config = {
    assigns = {
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
        criteria = {
          app_id = "^discord$";
          title = "^$";
        };
        command = "floating enable, sticky enable, border pixel 0, resize set 480 270, move position 1004 680, opacity 0.8";
      }
    ];
  };
}
