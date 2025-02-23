{
  config,
  lib,
  pkgs,
  pkgsKiraNur,
  ...
}:

let
  swaymsg = "${config.wayland.windowManager.sway.package}/bin/swaymsg";

  grim = "${pkgs.grim}/bin/grim";
  playerctl = "${pkgs.playerctl}/bin/playerctl";
  slurp = "${pkgs.slurp}/bin/slurp";
  wl-copy = "${pkgs.wl-clipboard}/bin/wl-copy";
  wpctl = "${pkgs.wireplumber}/bin/wpctl";

  # Randomly choose a wallpaper in ~/Pictures/Wallpapers
  random-wallpaper = pkgs.writeShellScript "random-wallpaper" ''
    ${pkgs.findutils}/bin/find -L ~/Pictures/Wallpapers -type f | ${pkgs.coreutils}/bin/shuf -n 1
  '';

  lock = pkgs.writeShellScript "lock" ''
    args=(--daemonize --image `${random-wallpaper}`)

    ${pkgs.systemd}/bin/systemctl is-enabled fprintd.service
    if [ $? -le 1 ]; then
      args+=(--fingerprint)
    fi

    exec ${pkgsKiraNur.swaylock-fprintd}/bin/swaylock "''${args[@]}"
  '';

  # Turn off scaling on all displays
  scale-off = pkgs.writeShellScript "scale-off" ''
    rm -rf /tmp/scale-on
    mkdir /tmp/scale-on

    while read -r scale name; do
      echo "$scale" > "/tmp/scale-on/$name";
    done < <(${swaymsg} -r -t get_outputs | ${pkgs.jq}/bin/jq -r '.[] | "\(.scale) \(.make) \(.model) \(.serial)"')

    ${swaymsg} 'output * scale 1'
  '';

  # Turn on scaling on all displays
  scale-on = pkgs.writeShellScript "scale-on" ''
    for scale_file in /tmp/scale-on/*; do
      ${swaymsg} "output \"$(basename "$scale_file")\" scale $(${pkgs.coreutils}/bin/cat "$scale_file")"
    done
  '';

  # Turn off scaling on all displays for the duration of the wrapped program
  wrap-scale-off = pkgs.writeShellScriptBin "wrap-scale-off" ''
    ${scale-off}
    export MANGOHUD_CONFIGFILE=$HOME/.config/MangoHud/MangoHud-HiDPI.conf
    "$1" "''${@:2}"
    ${scale-on}
  '';

  sound = pkgs.writeShellScript "sound" ''
    exec ${pkgs.vorbis-tools}/bin/ogg123 "${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/$1.oga"
  '';
in
{
  imports = [
    ./.

    # Administration
    ../../programs/dconf-editor

    # Media & Documents
    ../../programs/evince
    ../../programs/loupe
    ../../programs/mpv
    ../../programs/nautilus

    # Utils
    ../../programs/alacritty
    ../../programs/mako
    ../../programs/rofi
    ../../programs/waybar

    # Themes
    ../../environments/gtk
    ../../environments/qt
  ];

  wayland.windowManager.sway = {
    enable = true;
    package = pkgs.swayfx;
    checkConfig = false;
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
          names = [ "sans-serif" ];
          size = 10.0;
        };

        colors = {
          focused = {
            background = "#1b202b";
            border = "#1b202b";
            childBorder = "#2b303b";
            text = "#eeeeee";
            indicator = "#ff0000";
          };

          unfocused = {
            background = "#2b303b";
            border = "#2b303b";
            childBorder = "#2b303b";
            text = "#64727d";
            indicator = "#ff0000";
          };
        };

        output = {
          "*" = {
            bg = "`${random-wallpaper}` fill";
          };
        };

        seat = {
          "*" = {
            xcursor_theme = "${config.home.pointerCursor.name} ${toString config.home.pointerCursor.size}";
          };
        };

        startup = [ { command = "${pkgs.sway-audio-idle-inhibit}/bin/sway-audio-idle-inhibit"; } ];

        keybindings = lib.mkOptionDefault {
          "${cfg.modifier}+a" = "exec ${cfg.menu}";
          "${cfg.modifier}+q" = "kill";
          "${cfg.modifier}+Shift+q" = ''
            exec swaynag -t warning -m 'You pressed the exit shortcut. \
              Do you really want to exit sway? \
              This will end your Wayland session.' \
               -b 'Yes, exit sway' '${swaymsg} exit'
          '';

          "${cfg.modifier}+h" = "focus left";
          "${cfg.modifier}+n" = "focus down";
          "${cfg.modifier}+e" = "focus up";
          "${cfg.modifier}+i" = "focus right";

          "${cfg.modifier}+Shift+h" = "move left";
          "${cfg.modifier}+Shift+n" = "move down";
          "${cfg.modifier}+Shift+e" = "move up";
          "${cfg.modifier}+Shift+i" = "move right";

          "${cfg.modifier}+Control+Shift+h" = "move workspace to output left";
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

          "${cfg.modifier}+equal" = "exec ${scale-on}";
          "${cfg.modifier}+minus" = "exec ${scale-off}";

          "Print" = ''
            exec ${grim} -t png - \
              | ${wl-copy} -t image/png \
              & ${sound} screen-capture
          '';

          "Ctrl+Print" = ''
            exec ${grim} -t png \
              -g "$(${slurp})" - \
              | ${wl-copy} -t image/png \
              && ${sound} screen-capture
          '';

          "${cfg.modifier}+Print" = ''
            exec ${grim} -t png \
              "$HOME/Pictures/Screenshots/$(date +'Screenshot from %Y-%m-%d %T.png')" \
              | ${wl-copy} -t image/png \
              & ${sound} screen-capture
          '';

          "Ctrl+${cfg.modifier}+Print" = ''
            exec ${grim} -t png \
              -g "$(${slurp})" \
              "$HOME/Pictures/Screenshots/$(date +'Screenshot from %Y-%m-%d %T.png')" \
              | ${wl-copy} -t image/png \
              && ${sound} screen-capture'';

          "${cfg.modifier}+l" = "exec ${lock}";

          "XF86AudioRaiseVolume" = ''
            exec ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%+ -l 1 \
              & ${sound} audio-volume-change
          '';

          "XF86AudioLowerVolume" = ''
            exec ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%- \
              & ${sound} audio-volume-change
          '';

          "XF86AudioMute" = "exec ${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle";
          "Shift+XF86AudioMute" = "exec ${wpctl} set-mute @DEFAULT_AUDIO_SOURCE@ toggle";

          "XF86AudioPlay" = "exec ${playerctl} play-pause";
          "XF86AudioPrev" = "exec ${playerctl} previous";
          "XF86AudioNext" = "exec ${playerctl} next";
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
        command = "${swaymsg} 'output * power off'";
        resumeCommand = "${swaymsg} 'output * power on'";
      }
    ];
  };

  programs.waybar.settings.mainBar = {
    cpu = {
      on-click = "${pkgs.resources}/bin/resources";
    };

    memory = {
      on-click = "${pkgs.resources}/bin/resources";
    };

    disk = {
      on-click = "${pkgs.resources}/bin/resources";
    };

    temperature = {
      on-click = "${pkgs.resources}/bin/resources";
    };

    wireplumber = {
      on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
      on-click-right = "${pkgs.helvum}/bin/helvum";
    };
  };

  programs.gnome-pomodoro-swayidle = (
    lib.fix (self: {
      onstart = [
        "${swaymsg} 'gaps inner all set 0, bar mode hide'"
        "${sound} message-new-instant"
      ];

      onend = [ "${swaymsg} 'gaps inner all set 10, bar mode dock'" ];
      onresume = self.onstart;
      onpause = self.onend;
    })
  );

  home.packages = with pkgs; [
    # Administration
    baobab
    bustle
    d-spy
    helvum
    pavucontrol
    resources
    wl-mirror

    # Media & Documents
    file-roller

    # Utils
    gnome-clocks
    sway-audio-idle-inhibit
    wdisplays
    wlprop
    wrap-scale-off
  ];

  xdg.mimeApps.defaultApplications = {
    "inode/directory" = "emacsclient.desktop";
  };
}
