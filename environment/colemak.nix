{ config, pkgs, ... }:

{
  services.xserver = {
    layout = "us,us";
    xkbVariant = "colemak,";
    xkbOptions = "grp:win_space_toggle";
  };

  console.useXkbConfig = true;
}
