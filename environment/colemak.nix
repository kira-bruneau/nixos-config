{ config, pkgs, ... }:

{
  i18n = {
    consoleKeyMap = "/etc/nixos/colemak.map";
  };

  services.xserver = {
    layout = "us,us";
    xkbVariant = "colemak,";
    xkbOptions = "grp:win_space_toggle";
  };
}
