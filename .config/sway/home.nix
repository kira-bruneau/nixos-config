{ config, pkgs, ... }:

{
  imports = [
    ../../../alacritty/.config/alacritty/home.nix
  ];

  home.packages = with pkgs; with nur.repos.metadark; [
    alsaUtils
    glib.bin # gsettings
    grim
    keepassxc
    libnotify
    light
    pavucontrol
    rofi-wayland
    slurp
    sound-theme-freedesktop
    sox
    waybar
    wl-clipboard
    xdg-user-dirs
  ];

  programs.mako = {
    enable = true;
    defaultTimeout = 10000;
  };
}
