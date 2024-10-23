{
  config,
  pkgs,
  pkgsNixSoftwareCenter,
  pkgsUnstable,
  ...
}:

{
  imports = [
    ./.

    # Administration
    ../../programs/dconf-editor

    # Media & Documents
    ../../programs/evince
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
      d-spy
      gnome.gnome-disk-utility
      pkgsNixSoftwareCenter.nix-software-center
      resources

      # Media & Documents
      gnome.file-roller

      # Utils
      gnome-console
      gnome.gnome-clocks
      gnome.seahorse
    ])
    ++ (with pkgsUnstable; [
      # Extensions
      gnomeExtensions.dash-to-dock
      gnomeExtensions.random-wallpaper
    ]);

  programs = {
    bash.enableVteIntegration = true;
    gnome-pomodoro.package = pkgsUnstable.gnome.pomodoro;
    zsh.enableVteIntegration = true;
  };

  dconf.settings = {
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
    "org/gnome/shell" = {
      enabled-extensions = [
        "dash-to-dock@micxgx.gmail.com"
        "randomwallpaper@iflow.space"
      ];
    };
    # Dash to Dock settings for a better touch screen experience
    "org/gnome/shell/extensions/dash-to-dock" = {
      background-opacity = 0.8;
      custom-theme-shrink = true;
      dash-max-icon-size = 48;
      dock-fixed = true;
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
  };
}
