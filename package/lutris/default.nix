{ config, pkgs, ... }:

let
  settingsFormat = pkgs.formats.yaml { };
in
{
  home.packages = with pkgs; [
    lutris
  ];

  xdg.configFile = {
    # Manage lutris config outside of home-manager while keeping track of the files in this git repo
    "lutris/lutris.conf".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.configDirectory}/package/lutris/config/lutris.conf";

    # Manage lutris game configs outside of home-manager while keeping track of the files in this git repo
    "lutris/games".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.configDirectory}/package/lutris/config/games";

    "lutris/runners/dolphin.yml".source = settingsFormat.generate "dolphin.yml" {
      dolphin = {
        nogui = true;
        runner_executable = "${pkgs.dolphinEmuMaster}/bin/dolphin-emu";
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

    "lutris/runners/mupen64plus.yml".source = settingsFormat.generate "mupen64plus.yml" {
      mupen64plus = {
        runner_executable = "${pkgs.mupen64plus}/bin/mupen64plus";
      };

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

  wayland.windowManager.sway.config.assigns."5" = [
    { app_id = "^lutris$"; }
  ];
}
