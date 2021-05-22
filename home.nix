{ config, pkgs, ... }:

let
  # Usage: import-gsettings <gsettings key>:<settings.ini key> <gsettings key>:<settings.ini key> ...
  import-gsettings = pkgs.writeShellScriptBin "import-gsettings" ''
    expression=""
    for pair in "$@"; do
        IFS=:; set -- $pair
        expressions="$expressions -e 's:^$2=(.*)$:${pkgs.glib.bin}/bin/gsettings set org.gnome.desktop.interface $1 \1:e'"
    done
    IFS=
    eval exec ${pkgs.gnused}/bin/sed -E $expressions "''${XDG_CONFIG_HOME:-$HOME/.config}"/gtk-3.0/settings.ini >/dev/null
  '';

  # Randomly choose a wallpaper in ~/Pictures/Wallpapers
  random-wallpaper = pkgs.writeScriptBin "random-wallpaper" ''
    #!${pkgs.python3}/bin/python

    import glob
    import os
    import random
    from pathlib import Path

    wallpapers = os.path.join(Path.home(), 'Pictures/Wallpapers')
    print(random.choice(glob.glob("{}/*.*".format(wallpapers))))
  '';

  # Turn off scaling on all displays
  scale-off = pkgs.writeShellScriptBin "scale-off" ''
    ${pkgs.sway}/bin/swaymsg 'output "Goldstar Company Ltd LG HDR 4K 0x0000B721" scale 1'
  '';

  # Turn on scaling on all displays
  scale-on = pkgs.writeShellScriptBin "scale-on" ''
    ${pkgs.sway}/bin/swaymsg 'output "Goldstar Company Ltd LG HDR 4K 0x0000B721" scale 2'
  '';

  # Turn off scaling on all displays for the duration of the wrapped program
  wrap-scale-off = pkgs.writeShellScriptBin "wrap-scale-off" ''
    ${scale-off}/bin/scale-off
    "$1" "''${@:2}"
    ${scale-on}/bin/scale-on
  '';
in
{
  imports = [
    ../../../alacritty/.config/alacritty/home.nix
  ];

  home.packages = with pkgs; with nur.repos.metadark; [
    alsaUtils
    grim
    import-gsettings
    keepassxc
    libnotify
    light
    pavucontrol
    random-wallpaper
    rofi-wayland
    scale-off
    scale-on
    slurp
    sound-theme-freedesktop
    sox
    waybar
    wl-clipboard
    wrap-scale-off
    xdg-user-dirs
  ];

  programs.mako = {
    enable = true;
    defaultTimeout = 10000;
  };
}
