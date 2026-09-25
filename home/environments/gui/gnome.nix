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
    ++ (with pkgsUnstable; [
      # Extensions
      gnomeExtensions.dash-to-dock
      gnomeExtensions.random-wallpaper
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
        "dash-to-dock@micxgx.gmail.com"
        "randomwallpaper@iflow.space"
      ];
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
  };

  xdg.autostart.enable = true;
}
