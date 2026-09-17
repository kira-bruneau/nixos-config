{
  config,
  lib,
  pkgs,
  pkgsUnstable,
  ...
}:

{
  imports = [
    ./.

    # Administration
    ../../programs/dconf-editor

    # Media & Documents
    ../../programs/celluloid
    ../../programs/papers
    ../../programs/loupe
    ../../programs/nautilus

    # Themes
    ../../environments/gtk
    ../../environments/qt
  ];

  home.packages =
    (with pkgs; [
      # Administration
      baobab
      bustle
      d-spy
      gnome-disk-utility
      pkgsUnstable.helvum
      resources

      # Media & Documents
      file-roller
      foliate

      # Utils
      gnome-clocks
      gnome-console
      gnome-tecla
      gnome-weather
      seahorse
    ])
    ++ (with pkgsUnstable.gnomeExtensions; [
      # Extensions
      compiz-windows-effect
      dash-to-dock
      in-picture
      random-wallpaper
      ssh-search-provider-reborn
      syncthing-indicator
      tailscale-status
      vitals
      weather-oclock
    ]);

  programs = {
    bash.enableVteIntegration = true;
    gnome-pomodoro.package = pkgsUnstable.gnome-pomodoro;
    zsh.enableVteIntegration = true;
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      show-battery-percentage = true;
    };
    "org/gnome/desktop/wm/keybindings" = {
      close = [ "<Super>q" ];
      maximize = [ "<Super>equal" ];
      switch-windows = [ "<Super>w" ];
      switch-windows-backward = [ "<Shift><Super>w" ];
      unmaximize = [ "<Super>minus" ];
    };
    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enabled = true;
    };
    "org/gnome/settings-daemon/plugins/housekeeping" = {
      donation-reminder-enabled = false;
    };
    "org/gnome/settings-daemon/plugins/media-keys" = {
      control-center = [ "<Super>comma" ];
      home = [ "<Super>f" ];
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/terminal/"
      ];
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/terminal" = {
      name = "Open Terminal";
      command = lib.getExe pkgs.gnome-console;
      binding = "<Super>Return";
    };
    "org/gnome/settings-daemon/plugins/power" = {
      ambient-enabled = false;
    };
    "org/gnome/shell" = {
      enabled-extensions = [
        "compiz-windows-effect@hermes83.github.com"
        "dash-to-dock@micxgx.gmail.com"
        "in-picture@filiprund.cz"
        "randomwallpaper@iflow.space"
        "ssh-search-provider@extensions.gnome-shell.fifi.org"
        "syncthing@gnome.2nv2u.com"
        "tailscale-status@maxgallup.github.com"
        "Vitals@CoreCoding.com"
        "weatheroclock@CleoMenezesJr.github.io"
      ];
    };
    "org/gnome/shell/extensions/com/github/hermes83/compiz-windows-effect" = {
      maximize-effect = false;
    };
    "org/gnome/shell/extensions/dash-to-dock" = {
      background-opacity = 0.8;
      dash-max-icon-size = 48;
      dock-fixed = false;
      dock-position = "LEFT";
      extend-height = true;
      height-fraction = 0.61;
      hot-keys = false;
      preferred-monitor = -2;
      preferred-monitor-by-connector = "eDP-1";
      scroll-to-focused-application = true;
      show-apps-at-top = true;
      show-mounts = true;
      show-show-apps-button = true;
      show-trash = false;
    };
    "org/gnome/shell/extensions/in-picture" = {
      hide = true;
      stick = true;
    };
    "org/gnome/shell/extensions/space-iflow-randomwallpaper" = {
      hide-panel-icon = true;
      fetch-on-startup = true;
      sources = [ "0" ];
    };
    "org/gnome/shell/extensions/space-iflow-randomwallpaper/sources/general/0" = {
      name = "Wallpapers";
      type = 4; # Local folder
    };
    "org/gnome/shell/extensions/space-iflow-randomwallpaper/sources/localFolder/0" = {
      folder = "${config.home.homeDirectory}/Pictures/Wallpapers";
    };
    "org/gnome/shell/extensions/ssh-search-provider" = {
      terminal-application = "org.gnome.Console.desktop";
    };
    "org/gnome/shell/extensions/syncthing" = {
      menu = 1; # Main panel
      use-systemd = false; # SystemD integration doesn't work
    };
    "org/gnome/shell/extensions/vitals" = {
      icon-style = 1; # GNOME
      position-in-panel = 0; # Left
    };
    "org/gnome/shell/extensions/weather-oclock" = {
      weather-after-clock = true;
    };
  };

  xdg.autostart.enable = true;
}
