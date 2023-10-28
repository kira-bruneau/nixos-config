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
    ${pkgs.vorbis-tools}/bin/ogg123 "${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/$1.oga"
  '';
in
{
  imports = [
    ../alacritty
    ../keepassxc
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

            "${cfg.modifier}+l" = "exec ${pkgs.swaylock}/bin/swaylock --image `${random-wallpaper}`";

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

          startup = [
            { command = "${pkgs.anytype}/bin/anytype"; }
            { command = "${pkgs.blueman}/bin/blueman-applet"; }
            { command = "${config.programs.emacs.package}/bin/emacs"; }
            { command = "${config.programs.firefox.package}/bin/firefox"; }
            { command = "${pkgs.gnome.pomodoro}/bin/gnome-pomodoro --no-default-window --stop"; }
            { command = "${pkgs.newsflash}/bin/io.gitlab.news_flash.NewsFlash"; }
            { command = "${pkgs.keepassxc}/bin/keepassxc"; }
          ];

          assigns = {
            "1" = [
              { app_id = "^firefox$"; }
              { app_id = "^chromium-browser$"; }
            ];
            "2" = [
              { app_id = "^emacs"; }
              { app_id = "^codium-url-handler$"; }
            ];
            "4" = [
              { app_id = "^mpv$"; }
              { app_id = "^org.jellyfin.$"; }
              { app_id = "^org.qbittorrent.qBittorrent$"; }
            ];
            "5" = [
              { app_id = "^lutris$"; }
              { app_id = "^org.dolphin-emu.$"; }
              { app_id = "^org.prismlauncher.PrismLauncher$"; }
              { class = "^cemu.exe$"; }
              { class = "^dolphin-emu$"; } # Non-master Dolphin still uses X11
              { class = "^Minecraft$"; }
              { class = "^Steam$"; }
            ];
            "8" = [
              { app_id = "^anytype$"; }
            ];
            "9" = [
              { app_id = "^io.gitlab.news_flash.NewsFlash$"; }
            ];
            "10" = [
              { app_id = "^Caprine$"; }
              { app_id = "^Slack$"; }
            ];
          };

          window.commands = [
            {
              criteria = { app_id = "^firefox$"; title = "https://www.youtube.com"; };
              command = "move container to workspace 4";
            }
            {
              criteria = { app_id = "^firefox$"; title = "https://calendar.google.com"; };
              command = "move container to workspace 7";
            }
            {
              criteria = { app_id = "^firefox$"; title = "https://calendar.proton.me"; };
              command = "move container to workspace 7";
            }
            {
              criteria = { app_id = "^firefox$"; title = "https://mail.google.com"; };
              command = "move container to workspace 9";
            }
            {
              criteria = { app_id = "^firefox$"; title = "https://mail.proton.me"; };
              command = "move container to workspace 9";
            }
            {
              criteria = { app_id = "^firefox$"; title = "https://outlook.office.com"; };
              command = "move container to workspace 9";
            }
            {
              criteria = { app_id = "^firefox$"; title = "https://app.cinny.in"; };
              command = "move container to workspace 10";
            }
            {
              criteria = { app_id = "^firefox$"; title = "https://app.element.io"; };
              command = "move container to workspace 10";
            }
            {
              criteria = { app_id = "^firefox$"; title = "https://app.slack.com"; };
              command = "move container to workspace 10";
            }
            {
              criteria = { app_id = "^firefox$"; title = "^Picture-in-Picture$"; };
              command = "floating enable, sticky enable, border pixel 0, move position 1340 722, opacity 0.95";
            }
          ];

          bars = [
            {
              command = "${config.programs.waybar.package}/bin/waybar";
            }
          ];
        };

    extraConfig = ''
      blur enable
      corner_radius 10
      shadows enable
      shadows_on_csd enable
      layer_effects 'waybar' 'blur enable; shadows enable'
      layer_effects 'rofi' 'blur enable; shadows enable'
      include /etc/sway/config.d/*
      exec "${pkgs.dbus}/bin/dbus-update-activation-environment --systemd XCURSOR_PATH XCURSOR_NAME XCURSOR_SIZE"
    '';
  };

  home.packages = with pkgs; [
    wlprop
    wrap-scale-off
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
}
