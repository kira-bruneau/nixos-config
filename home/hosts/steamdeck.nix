{ lib, ... }:

{
  imports = [
    ../environments/bluetooth.nix
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
  ];

  home = {
    stateVersion = "23.11";
    packages = with pkgs; [
      gnomeExtensions.dash-to-dock
    ];
  };

  dconf.settings = {
    # Enable on-screen keyboard
    "org/gnome/desktop/a11y/applications" = {
      screen-keyboard-enabled = true;
    };
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

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
    "clonehero"
    "clonehero-unwrapped"
    "data.zip"
    "discord"
    "sm64ex"
    "steam"
    "steam-original"
    "steam-run"
    "unrar"
    "vvvvvv"
  ];
}
