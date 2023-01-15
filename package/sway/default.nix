{ pkgs, ... }:

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
  scale-off = pkgs.writeShellApplication {
    name = "scale-off";
    runtimeInputs = with pkgs; [ sway jq ];
    text = ''
      rm -rf /tmp/scale-on
      mkdir /tmp/scale-on

      while read -r scale name; do
        echo "$scale" > "/tmp/scale-on/$name";
      done < <(swaymsg -r -t get_outputs | jq -r '.[] | "\(.scale) \(.make) \(.model) \(.serial)"')

      swaymsg 'output * scale 1'
    '';
  };

  # Turn on scaling on all displays
  scale-on = pkgs.writeShellApplication {
    name = "scale-on";
    runtimeInputs = with pkgs; [ coreutils sway jq ];
    text = ''
      for scale_file in /tmp/scale-on/*; do
        swaymsg "output \"$(basename "$scale_file")\" scale $(cat "$scale_file")"
      done
    '';
  };

  # Turn off scaling on all displays for the duration of the wrapped program
  wrap-scale-off = pkgs.writeShellApplication {
    name = "wrap-scale-off";
    runtimeInputs = [ scale-off scale-on ];
    text = ''
      scale-off
      export MANGOHUD_CONFIGFILE=$HOME/.config/MangoHud/MangoHud-HiDPI.conf
      "$1" "''${@:2}"
      scale-on
    '';
  };
in
{
  imports = [
    ../alacritty
    ../rofi
    ../waybar
  ];

  home.packages = with pkgs; [
    alsa-utils
    grim
    import-gsettings
    keepassxc
    libnotify
    light
    pavucontrol
    random-wallpaper
    scale-off
    scale-on
    slurp
    sound-theme-freedesktop
    sox
    wl-clipboard
    wrap-scale-off
    xdg-user-dirs
  ];

  programs.mako = {
    enable = true;
    defaultTimeout = 10000;
    extraConfig = ''
      [mode=invisible]
      invisible=1

      [mode=sticky]
      ignore-timeout=true
      default-timeout=0
    '';
  };

  # TODO: Generate configuration from Nix
  xdg.configFile.sway.source = ./config;
}
