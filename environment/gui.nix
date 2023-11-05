{ pkgs, ... }:

{
  imports = [
    # Window managers
    ../package/sway

    # Web
    ../package/firefox
    ../package/newsflash

    # Media & Documents
    ../package/emacs
    ../package/eog
    ../package/evince
    ../package/keepassxc
    ../package/lmms
    ../package/mpv

    # Themes
    ../package/gtk

    # Utils
    ../package/speedcrunch
    ../package/gnome-pomodoro
  ];

  home.packages = with pkgs; [
    # Administration
    gnome.dconf-editor
    pavucontrol

    # Web
    jellyfin-media-player
    qbittorrent
    syncplay
    ungoogled-chromium

    # Chat
    discord

    # Media & Documents
    anytype
    audacity
    gimp
    gnome.file-roller
    gnome.nautilus
    gnucash
    inkscape
    libreoffice
    sqlitebrowser
    xournalpp
    zynaddsubfx

    # Utils
    gnome.gnome-clocks
    libnotify
    yabridge
    yabridgectl
  ];

  home = {
    pointerCursor = {
      package = pkgs.gnome.adwaita-icon-theme;
      name = "Adwaita";
      size = 24;
    };

    sessionVariables = {
      # Improve appearance of Java applications
      # https://wiki.archlinux.org/index.php/Java#Tips_and_tricks
      _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel";

      # Use Wayland for Chrome & Electron apps
      NIXOS_OZONE_WL = 1;
    };
  };

  xdg = {
    mimeApps.enable = true;

    # mimeapps.list often gets overwritten by applications adding mimetype associations
    configFile."mimeapps.list".force = true;
  };

  wayland.windowManager.sway.config = {
    startup = [
      { command = "${pkgs.anytype}/bin/anytype"; }
    ];

    assigns = {
      "1" = [
        { app_id = "^chromium-browser$"; }
      ];
      "4" = [
        { app_id = "^org.jellyfin.$"; }
        { app_id = "^org.qbittorrent.qBittorrent$"; }
        { title = "^Syncplay"; }
      ];
      "8" = [
        { app_id = "^anytype$"; }
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
      {
        criteria = { app_id = "^org.jellyfin.$"; };
        command = "inhibit_idle fullscreen";
      }
    ];
  };

  services.gpg-agent.pinentryFlavor = "gnome3";

  services.blueman-applet.enable = true;

  services.easyeffects.enable = true;
}
