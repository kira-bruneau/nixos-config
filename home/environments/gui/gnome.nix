{ pkgs, ... }:

{
  imports = [
    ./.

    # Administration
    ../../programs/dconf-editor

    # Media & Documents
    ../../programs/evince
    ../../programs/loupe

    # Themes
    ../../programs/gtk
  ];

  home.packages = with pkgs; [
    # Administration
    baobab
    gnome.gnome-disk-utility
    gnome.gnome-system-monitor

    # Media & Documents
    gnome.file-roller
    gnome.nautilus

    # Utils
    gnome-console
    gnome.gnome-clocks
    gnome.seahorse
  ];

  programs.bash.enableVteIntegration = true;
  programs.zsh.enableVteIntegration = true;

  xdg.mimeApps.defaultApplications = {
    "inode/directory" = "nautilus.desktop";
  };

  dconf.settings = {
    "org/gnome/shell" = {
      enabled-extensions = [
        "dash-to-dock@micxgx.gmail.com"
      ];
    };
    # Dash to Dock settings for a better touch screen experience
    "org/gnome/shell/extensions/dash-to-dock" = {
      background-opacity = 0.80000000000000004;
      custom-theme-shrink = true;
      dash-max-icon-size = 48;
      dock-fixed = true;
      dock-position = "LEFT";
      extend-height = true;
      height-fraction = 0.60999999999999999;
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
