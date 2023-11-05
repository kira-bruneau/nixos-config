{ lib, config, pkgs, ... }:

let
  # Randomly choose a wallpaper in ~/Pictures/Wallpapers
  random-wallpaper = pkgs.writeScript "random-wallpaper" ''
    #!${pkgs.python3}/bin/python

    import glob
    import os
    import random
    from pathlib import Path

    wallpapers = os.path.join(Path.home(), 'Pictures/Wallpapers')
    print(random.choice(glob.glob("{}/*.*".format(wallpapers))))
  '';

  lock = pkgs.writeShellScript "lock" ''
    exec ${pkgs.swaylock}/bin/swaylock --daemonize --image `${random-wallpaper}`
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

  sound = pkgs.writeShellScript "sound" ''
    exec ${pkgs.vorbis-tools}/bin/ogg123 "${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/$1.oga"
  '';
in
{
  imports = [
    ../alacritty
    ../keepassxc
    ../mako
    ../rofi
    ../waybar
  ];

  wayland.windowManager.sway = {
    enable = true;
    config =
      let
        cfg = config.wayland.windowManager.sway.config;
      in
        {
          modifier = "Mod4";

          gaps.inner = 10;

          window = {
            titlebar = false;
            border = 0;
          };

          floating.border = 0;

          fonts = {
            names = [ "cantarell" ];
            size = 10.0;
          };

          output = {
            "*" = {
              bg = "`${random-wallpaper}` fill";
            };
          };

          seat = lib.mkIf (config.home.pointerCursor != null) {
            "*" = {
              # TODO: Fractional scaling size doesn't match other app cursors
              # https://github.com/swaywm/sway/issues/5202
              xcursor_theme = "${config.home.pointerCursor.name} ${toString config.home.pointerCursor.size}";
            };
          };

          keybindings = lib.mkOptionDefault {
            "${cfg.modifier}+a" = "exec ${cfg.menu}";
            "${cfg.modifier}+q" = "kill";
            "${cfg.modifier}+Shift+q" = ''
              exec swaynag -t warning -m 'You pressed the exit shortcut. \
                Do you really want to exit sway? \
                This will end your Wayland session.' \
                 -b 'Yes, exit sway' 'swaymsg exit'
            '';

            "${cfg.modifier}+h" = "focus left";
            "${cfg.modifier}+n" = "focus down";
            "${cfg.modifier}+e" = "focus up";
            "${cfg.modifier}+i" = "focus right";

            "${cfg.modifier}+Shift+h" = "move left";
            "${cfg.modifier}+Shift+n" = "move down";
            "${cfg.modifier}+Shift+e" = "move up";
            "${cfg.modifier}+Shift+i" = "move right";

            "${cfg.modifier}+Control+Shift+n" = "move workspace to output down";
            "${cfg.modifier}+Control+Shift+e" = "move workspace to output up";
            "${cfg.modifier}+Control+Shift+i" = "move workspace to output right";

            "${cfg.modifier}+s" = "layout stacking";
            "${cfg.modifier}+t" = "layout tabbed";
            "${cfg.modifier}+d" = "layout toggle split";

            "${cfg.modifier}+comma" = "focus parent";
            "${cfg.modifier}+period" = "focus child";

            # Mod+space switches keyboard layout
            "${cfg.modifier}+Shift+space" = null;
            "${cfg.modifier}+space" = null;

            "${cfg.modifier}+Shift+f" = "floating toggle";
            "${cfg.modifier}+Control+f" = "focus mode_toggle";

            "${cfg.modifier}+0" = "workspace number 10";
            "${cfg.modifier}+Shift+0" = "move container to workspace number 10";

            "${cfg.modifier}+Shift+slash" = "move scratchpad";
            "${cfg.modifier}+slash" = "scratchpad show";

            "${cfg.modifier}+equal" = "exec ${scale-on}/bin/scale-on";
            "${cfg.modifier}+minus" = "exec ${scale-off}/bin/scale-off";

            "Print" = ''
              exec ${pkgs.grim}/bin/grim -t png - \
                | ${pkgs.wl-clipboard}/bin/wl-copy -t image/png \
                & ${sound} screen-capture
            '';

            "Ctrl+Print" = ''
              exec ${pkgs.grim}/bin/grim -t png \
                -g "$(${pkgs.slurp}/bin/slurp)" - \
                | ${pkgs.wl-clipboard}/bin/wl-copy -t image/png \
                && ${sound} screen-capture
            '';

            "${cfg.modifier}+Print" = ''
              exec ${pkgs.grim}/bin/grim -t png \
                "$HOME/Pictures/Screenshots/$(date +'Screenshot from %Y-%m-%d %T.png')" \
                | ${pkgs.wl-clipboard}/bin/wl-copy -t image/png \
                & ${sound} screen-capture
            '';

            "Ctrl+${cfg.modifier}+Print" = ''
              exec ${pkgs.grim}/bin/grim -t png \
                -g "$(${pkgs.slurp}/bin/slurp)" \
                "$HOME/Pictures/Screenshots/$(date +'Screenshot from %Y-%m-%d %T.png')" \
                | ${pkgs.wl-clipboard}/bin/wl-copy -t image/png \
                && ${sound} screen-capture'';

            "${cfg.modifier}+l" = "exec ${lock}";

            "XF86AudioRaiseVolume" = ''
              exec ${pkgs.alsa-utils}/bin/amixer sset Master 5%+ \
                & ${sound} audio-volume-change
            '';

            "XF86AudioLowerVolume" = ''
              exec ${pkgs.alsa-utils}/bin/amixer sset Master 5%- \
                & ${sound} audio-volume-change
            '';

            "XF86AudioMute" = "exec ${pkgs.alsa-utils}/bin/amixer sset Master toggle";

            "${cfg.modifier}+XF86AudioMute" = "exec ${pkgs.alsa-utils}/bin/amixer Capture toggle";
          };

          modes = lib.mkOptionDefault {
            resize = {
              "h" = "resize shrink width 10 px";
              "n" = "resize grow height 10 px";
              "e" = "resize shrink height 10 px";
              "i" = "resize grow width 10 px";
            };
          };
        };

    extraConfig = ''
      blur enable
      corner_radius 10
      shadows enable
      shadows_on_csd enable
      include /etc/sway/config.d/*
      exec "${pkgs.dbus}/bin/dbus-update-activation-environment --systemd XCURSOR_PATH XCURSOR_NAME XCURSOR_SIZE"
    '';
  };

  services.swayidle = {
    enable = true;

    events = [
      {
        event = "before-sleep";
        command = "${lock}";
      }
    ];

    timeouts = [
      {
        timeout = 300;
        command = "${lock}";
      }
      {
        timeout = 600;
        command = "/run/current-system/sw/bin/swaymsg 'output * power off'";
        resumeCommand = "/run/current-system/sw/bin/swaymsg 'output * power on'";
      }
    ];
  };

  home.packages = with pkgs; [
    wlprop
    wrap-scale-off
  ];
}
