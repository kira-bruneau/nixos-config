{ pkgs, ... }:

{
  programs.steam = {
    enable = true;
    gamescopeSession.enable = true;
    localNetworkGameTransfers.openFirewall = true;
  };

  environment.sessionVariables.STEAMOS = "1";

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
