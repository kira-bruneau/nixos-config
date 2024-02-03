{ pkgs, ... }:

{
  programs.steam.enable = true;
  environment.variables.STEAMOS = "1";

  # Open ports for steam local network game transfers
  networking.firewall = {
    allowedTCPPorts = [ 27040 ];
    allowedUDPPortRanges = [ { from = 27031; to = 27036; } ];
  };

  # Better driver for Xbox One controllers
  hardware.xpadneo.enable = true;

  # Enable GameMode to optimise system performance on-demand
  programs.gamemode = {
    enable = true;
    settings = {
      general = {
        renice = 10;
      };

      custom = {
        start = "${pkgs.libnotify}/bin/notify-send --app-name GameMode 'GameMode started'";
        end = "${pkgs.libnotify}/bin/notify-send --app-name GameMode 'GameMode ended'";
      };
    };
  };

  # Enable ReplaySorcery for background screen recording & instant replays
  services.replay-sorcery = {
    enable = true;
    enableSysAdminCapability = true;
  };

  # Set higher file limit for wine esync support
  security.pam.loginLimits = [
    {
      domain = "*";
      type = "-";
      item = "nofile";
      value = "524288";
    }
  ];
}
