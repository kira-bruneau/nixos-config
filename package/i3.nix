{ config, pkgs, ... }:

{
  # Packages required by my i3 config
  environment.systemPackages = with pkgs; [
    compton
    dunst
    feh
    font-awesome_5
    geoclue2 # TODO: Allow redshift to use geoclue
    light
    polybar
    redshift
    rofi
    scrot
    tilix
    xcwd
  ];

  services.xserver = {
    enable = true;
    windowManager.i3.enable = true;
  };
}
