{ config, pkgs, ... }:

let
  settingsFormat = pkgs.formats.yaml { };
in
{
  home.packages = with pkgs; [ lutris-free ];

  xdg.configFile = {
    # Manage lutris config outside of home-manager while keeping track of the files in this git repo
    "lutris/lutris.conf".source = config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/lutris/config/lutris.conf";

    # Manage lutris game configs outside of home-manager while keeping track of the files in this git repo
    "lutris/games".source = config.lib.file.mkOutOfStoreSymlink "${config.home.configDirectory}/programs/lutris/config/games";

    "lutris/runners/dolphin.yml".source = settingsFormat.generate "dolphin.yml" {
      dolphin = {
        nogui = true;
        runner_executable = "${pkgs.dolphin-emu}/bin/dolphin-emu";
      };

      system = {
        disable_runtime = true;

        # Dolphin doesn't work on Wayland - force X11
        env = {
          QT_AUTO_SCREEN_SCALE_FACTOR = "1";
          QT_QPA_PLATFORM = "xcb";
        };

        prefix_command = "wrap-scale-off";
      };
    };

    "lutris/runners/linux.yml".source = settingsFormat.generate "linux.yml" {
      linux = { };

      system = {
        disable_runtime = true;
      };
    };

    "lutris/runners/steam.yml".source = settingsFormat.generate "steam.yml" {
      steam = { };

      system = {
        disable_runtime = true;

        # I've configured steam to only run gamemode while a game is running
        gamemode = false;
      };
    };

    "lutris/runners/wine.yml".source = settingsFormat.generate "wine.yml" {
      wine = { };

      system = {
        prefix_command = "wrap-scale-off";
      };
    };
  };

  wayland.windowManager.sway.config.assigns."5" = [ { app_id = "^lutris$"; } ];
}
