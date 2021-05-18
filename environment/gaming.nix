{ config, pkgs, ... }:

{
  # Support for running 32bit games
  # See https://nixos.wiki/wiki/Steam
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];

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
        start = "${pkgs.libnotify}/bin/notify-send 'GameMode started'";
        end = "${pkgs.libnotify}/bin/notify-send 'GameMode ended'";
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
      item = "nofile";
      type = "-";
      value = "524288";
    }
  ];
}
