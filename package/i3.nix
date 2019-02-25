{ config, pkgs, ... }:

{
  # Packages required by my i3 config
  environment.systemPackages = with pkgs; [
    alacritty
    compton
    dunst
    feh
    light
    polybar
    redshift
    rofi
    scrot
    xcwd
  ];

  fonts.fonts = with pkgs; [
    font-awesome_5
  ];

  services = {
    xserver = {
      enable = true;
      windowManager.i3.enable = true;
    };
    geoclue2.enable = true;
  };
}
