{ pkgs, ... }:

let
  # Source: https://github.com/swaywm/sway/wiki/GTK-3-settings-on-Wayland#setting-values-in-gsettings
  import-gsettings = pkgs.writeShellScriptBin "import-gsettings" ''
    config="''${XDG_CONFIG_HOME:-$HOME/.config}/gtk-3.0/settings.ini"
    if [ ! -f "$config" ]; then exit 1; fi

    gnome_schema="org.gnome.desktop.interface"
    gtk_theme="$(grep 'gtk-theme-name' "$config" | sed 's/.*\s*=\s*//')"
    icon_theme="$(grep 'gtk-icon-theme-name' "$config" | sed 's/.*\s*=\s*//')"
    cursor_theme="$(grep 'gtk-cursor-theme-name' "$config" | sed 's/.*\s*=\s*//')"
    font_name="$(grep 'gtk-font-name' "$config" | sed 's/.*\s*=\s*//')"
    gsettings set "$gnome_schema" gtk-theme "$gtk_theme"
    gsettings set "$gnome_schema" icon-theme "$icon_theme"
    gsettings set "$gnome_schema" cursor-theme "$cursor_theme"
    gsettings set "$gnome_schema" font-name "$font_name"
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
    font-awesome_5
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
