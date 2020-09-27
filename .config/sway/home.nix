{ config, pkgs, ... }:

{
  imports = [
    ../../../alacritty/.config/alacritty/home.nix
  ];

  home.packages = with pkgs; with nur.repos.metadark; [
    dunst
    glib.bin # gsettings
    grim
    keepassxc
    light
    pavucontrol
    rofi-wayland
    slurp
    sound-theme-freedesktop
    (waybar.override {
      pulseSupport = true;
      mpdSupport = false;
    })
    wl-clipboard
    xdg-user-dirs
  ];
}
