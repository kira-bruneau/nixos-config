{ pkgs, ... }:

let
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
    ../keepassxc
    ../rofi
    ../waybar
  ];

  home.packages = with pkgs; [
    alsa-utils
    font-awesome_5
    grim
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

  services.mako = {
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
