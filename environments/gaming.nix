{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.steam = {
    enable = true;
    gamescopeSession.enable = true;
    localNetworkGameTransfers.openFirewall = true;
  };

  environment.sessionVariables = {
    STEAMOS = "1";

    # Fix keyboard layout in gamescope
    # Source: https://github.com/ValveSoftware/gamescope/issues/203
    XKB_DEFAULT_LAYOUT = builtins.head (lib.splitString "," config.services.xserver.xkb.layout);
    XKB_DEFAULT_VARIANT = builtins.head (lib.splitString "," config.services.xserver.xkb.variant);
  };

  # Enable GameMode to optimise system performance on-demand
  programs.gamemode = {
    enable = true;
    settings = {
      general = {
        renice = 10;
      };

      custom = {
        start = [ "${pkgs.libnotify}/bin/notify-send --app-name GameMode 'GameMode started'" ];
        end = [ "${pkgs.libnotify}/bin/notify-send --app-name GameMode 'GameMode ended'" ];
      };
    };
  };
}
