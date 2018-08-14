{ config, pkgs, ... }:

{
  # Packages required by my i3 config
  environment.systemPackages = with pkgs; [
    compton
    dunst
    feh
    geoclue2 # TODO: Allow redshift to use geoclue
    light
    polybar
    redshift
    rofi
    scrot
    tilix
  ];

  services.xserver = {
    enable = true;
    windowManager.i3.enable = true;
  };
}
