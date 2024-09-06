{ lib, pkgs, ... }:

{
  imports = [
    ../../environments/wallpapers.nix

    # Web
    ../../programs/jellyfin
    ../../programs/librewolf

    # Media & Documents
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
      yt-dlp

      # Chat
      discord

      # Fonts
      ubuntu-sans
      ubuntu-sans-mono

      # Utils
      libnotify
      xdg-utils
    ];

    pointerCursor = {
      package = pkgs.adwaita-icon-theme;
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
    defaultFonts.sansSerif = [ "Ubuntu Sans" ];
    defaultFonts.monospace = [ "Ubuntu Sans Mono" ];
  };

  # TODO THIS
  # xdg.configFile."wireplumber/main.lua.d/51-restrict-cotrol.lua".text = ''
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
  #         { "application.process.binary", "matches", "*librewolf*" },
  #       },
  #     },
  #     default_permissions = "rx",
  #   })
  # '';

  wayland.windowManager.sway.config = {
    assigns = {
      "4" = [ { app_id = "^org.qbittorrent.qBittorrent$"; } ];
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

  dconf.enable = true;

  systemd.user.services.taildrop = {
    Install.WantedBy = [ "default.target" ];
    Unit.Description = "Automatically save taildrop files to ~/Downloads/Taildrop";
    Service = {
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p '%h/Downloads/Taildrop'";
      ExecStart = "${lib.getExe pkgs.tailscale} file get --loop '%h/Downloads/Taildrop'";
    };
  };
}
