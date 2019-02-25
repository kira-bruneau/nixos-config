{ config, pkgs, ... }:

{
  i18n = {
    consoleKeyMap = "colemak";
  };

  services.xserver = {
    layout = "us,us";
    xkbVariant = "colemak,";
    xkbOptions = "grp:win_space_toggle";
  };
}
