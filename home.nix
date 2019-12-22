{ config, pkgs, ... }:

{
  imports = [
    ../../../alacritty/.config/alacritty/home.nix
  ];

  home.packages = with pkgs; [
    dunst
    grim
    keepassxc
    light
    pavucontrol
    rofi
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
