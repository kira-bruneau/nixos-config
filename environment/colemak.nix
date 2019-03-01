{ config, pkgs, ... }:

{
  services.xserver = {
    layout = "us,us";
    xkbVariant = "colemak,";
    xkbOptions = "grp:win_space_toggle";
  };

  i18n.consoleUseXkbConfig = true;
}
