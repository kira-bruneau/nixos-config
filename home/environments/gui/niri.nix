{
  lib,
  pkgs,
  pkgsKiraNur,
  ...
}:

let
  brightnessctl = lib.getExe pkgs.brightnessctl;

  random-wallpaper = pkgs.writeShellScript "random-wallpaper" ''
    ${lib.getExe pkgs.findutils} -L ~/Pictures/Wallpapers -type f | ${pkgs.coreutils}/bin/shuf -n 1
  '';

  lock = pkgs.writeShellScript "lock" ''
    args=(--daemonize --image `${random-wallpaper}`)

    ${pkgs.systemd}/bin/systemctl is-enabled fprintd.service
    if [ $? -le 1 ]; then
      args+=(--fingerprint)
    fi

    exec ${pkgsKiraNur.swaylock-fprintd}/bin/swaylock "''${args[@]}"
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

  programs.niri.settings = {
    hotkey-overlay.skip-at-startup = true;

    input = {
      # colemak
      keyboard = {
        xkb = {
          layout = "us,us";
          variant = "colemak,";
          options = "grp:win_space_toggle";
        };
      };
    };

    outputs = {
      # lapis
      "BOE 0x095F Unknown".scale = 1.5;
      "LG Electronics LG HDR 4K 0x0005B621".scale = 2;
      "Technical Concepts Ltd 65S535CA Unknown".scale = 2;
    };

    binds = {
      # laptop
      "XF86MonBrightnessUp".action.spawn = [
        brightnessctl
        "set"
        "5%+"
      ];
      "Ctrl+XF86MonBrightnessUp".action.spawn = [
        brightnessctl
        "set"
        "1%+"
      ];
      "XF86MonBrightnessDown".action.spawn = [
        brightnessctl
        "set"
        "5%-"
      ];
      "Ctrl+XF86MonBrightnessDown".action.spawn = [
        brightnessctl
        "set"
        "1%-"
      ];
      "Shift+XF86MonBrightnessUp".action.spawn = [
        brightnessctl
        "set"
        "100%"
      ];
      "Shift+XF86MonBrightnessDown".action.spawn = [
        brightnessctl
        "set"
        "1"
      ];

      # niri
      "Ctrl+Alt+Delete".action.spawn = lib.getExe pkgs.wleave;

      # Keys consist of modifiers separated by + signs, followed by an XKB key name
      # in the end. To find an XKB name for a particular key, you may use a program
      # like wev.
      #
      # "Mod" is a special modifier equal to Super when running on a TTY, and to Alt
      # when running as a winit window.
      #
      # Most actions that you can bind here can also be invoked programmatically with
      # `niri msg action do-something`.

      # Mod-Shift-/, which is usually the same as Mod-?,
      # shows a list of important hotkeys.
      "Mod+Shift+Slash".action.show-hotkey-overlay = { };

      "Super+Alt+L".action.spawn = "${lock}";

      # Example volume keys mappings for PipeWire & WirePlumber.
      # The allow-when-locked=true property makes them work even when the session is locked.
      "XF86AudioRaiseVolume" = {
        allow-when-locked = true;
        action.spawn = [
          "wpctl"
          "set-volume"
          "@DEFAULT_AUDIO_SINK@"
          "0.1+"
        ];
      };
      "XF86AudioLowerVolume" = {
        allow-when-locked = true;
        action.spawn = [
          "wpctl"
          "set-volume"
          "@DEFAULT_AUDIO_SINK@"
          "0.1-"
        ];
      };
      "XF86AudioMute" = {
        allow-when-locked = true;
        action.spawn = [
          "wpctl"
          "set-mute"
          "@DEFAULT_AUDIO_SINK@"
          "toggle"
        ];
      };
      "XF86AudioMicMute" = {
        allow-when-locked = true;
        action.spawn = [
          "wpctl"
          "set-mute"
          "@DEFAULT_AUDIO_SOURCE@"
          "toggle"
        ];
      };

      "Mod+Q".action.close-window = { };

      "Mod+Left".action.focus-column-left = { };
      "Mod+Down".action.focus-window-down = { };
      "Mod+Up".action.focus-window-up = { };
      "Mod+Right".action.focus-column-right = { };
      "Mod+H".action.focus-column-left = { };
      "Mod+N".action.focus-window-down = { };
      "Mod+E".action.focus-window-up = { };
      "Mod+I".action.focus-column-right = { };

      "Mod+Ctrl+Left".action.move-column-left = { };
      "Mod+Ctrl+Down".action.move-window-down = { };
      "Mod+Ctrl+Up".action.move-window-up = { };
      "Mod+Ctrl+Right".action.move-column-right = { };
      "Mod+Ctrl+H".action.move-column-left = { };
      "Mod+Ctrl+N".action.move-window-down = { };
      "Mod+Ctrl+E".action.move-window-up = { };
      "Mod+Ctrl+I".action.move-column-right = { };

      # Alternative commands that move across workspaces when reaching
      # the first or last window in a column.
      # Mod+N     { focus-window-or-workspace-down; }
      # Mod+E     { focus-window-or-workspace-up; }
      # Mod+Ctrl+N     { move-window-down-or-to-workspace-down; }
      # Mod+Ctrl+E     { move-window-up-or-to-workspace-up; }

      "Mod+Home".action.focus-column-first = { };
      "Mod+End".action.focus-column-last = { };
      "Mod+Ctrl+Home".action.move-column-to-first = { };
      "Mod+Ctrl+End".action.move-column-to-last = { };

      "Mod+Shift+Left".action.focus-monitor-left = { };
      "Mod+Shift+Down".action.focus-monitor-down = { };
      "Mod+Shift+Up".action.focus-monitor-up = { };
      "Mod+Shift+Right".action.focus-monitor-right = { };
      "Mod+Shift+H".action.focus-monitor-left = { };
      "Mod+Shift+N".action.focus-monitor-down = { };
      "Mod+Shift+E".action.focus-monitor-up = { };
      "Mod+Shift+I".action.focus-monitor-right = { };

      "Mod+Shift+Ctrl+Left".action.move-column-to-monitor-left = { };
      "Mod+Shift+Ctrl+Down".action.move-column-to-monitor-down = { };
      "Mod+Shift+Ctrl+Up".action.move-column-to-monitor-up = { };
      "Mod+Shift+Ctrl+Right".action.move-column-to-monitor-right = { };
      "Mod+Shift+Ctrl+H".action.move-column-to-monitor-left = { };
      "Mod+Shift+Ctrl+N".action.move-column-to-monitor-down = { };
      "Mod+Shift+Ctrl+E".action.move-column-to-monitor-up = { };
      "Mod+Shift+Ctrl+I".action.move-column-to-monitor-right = { };

      # Alternatively, there are commands to move just a single window:
      # Mod+Shift+Ctrl+Left  { move-window-to-monitor-left; }
      # ...

      # And you can also move a whole workspace to another monitor:
      # Mod+Shift+Ctrl+Left  { move-workspace-to-monitor-left; }
      # ...

      "Mod+Page_Down".action.focus-workspace-down = { };
      "Mod+Page_Up".action.focus-workspace-up = { };
      "Mod+L".action.focus-workspace-down = { };
      "Mod+U".action.focus-workspace-up = { };
      "Mod+Ctrl+Page_Down".action.move-column-to-workspace-down = { };
      "Mod+Ctrl+Page_Up".action.move-column-to-workspace-up = { };
      "Mod+Ctrl+L".action.move-column-to-workspace-down = { };
      "Mod+Ctrl+U".action.move-column-to-workspace-up = { };

      # Alternatively, there are commands to move just a single window:
      # Mod+Ctrl+Page_Down { move-window-to-workspace-down; }
      # ...

      "Mod+Shift+Page_Down".action.move-workspace-down = { };
      "Mod+Shift+Page_Up".action.move-workspace-up = { };
      "Mod+Shift+L".action.move-workspace-down = { };
      "Mod+Shift+U".action.move-workspace-up = { };

      # You can bind mouse wheel scroll ticks using the following syntax.
      # These binds will change direction based on the natural-scroll setting.
      #
      # To avoid scrolling through workspaces really fast, you can use
      # the cooldown-ms property. The bind will be rate-limited to this value.
      # You can set a cooldown on any bind, but it's most useful for the wheel.
      "Mod+WheelScrollDown" = {
        cooldown-ms = 150;
        action.focus-workspace-down = { };
      };
      "Mod+WheelScrollUp" = {
        cooldown-ms = 150;
        action.focus-workspace-up = { };
      };
      "Mod+Ctrl+WheelScrollDown" = {
        cooldown-ms = 150;
        action.move-column-to-workspace-down = { };
      };
      "Mod+Ctrl+WheelScrollUp" = {
        cooldown-ms = 150;
        action.move-column-to-workspace-up = { };
      };

      "Mod+WheelScrollRight".action.focus-column-right = { };
      "Mod+WheelScrollLeft".action.focus-column-left = { };
      "Mod+Ctrl+WheelScrollRight".action.move-column-right = { };
      "Mod+Ctrl+WheelScrollLeft".action.move-column-left = { };

      # Usually scrolling up and down with Shift in applications results in
      # horizontal scrolling; these binds replicate that.
      "Mod+Shift+WheelScrollDown".action.focus-column-right = { };
      "Mod+Shift+WheelScrollUp".action.focus-column-left = { };
      "Mod+Ctrl+Shift+WheelScrollDown".action.move-column-right = { };
      "Mod+Ctrl+Shift+WheelScrollUp".action.move-column-left = { };

      # Similarly, you can bind touchpad scroll "ticks".
      # Touchpad scrolling is continuous, so for these binds it is split into
      # discrete intervals.
      # These binds are also affected by touchpad's natural-scroll, so these
      # example binds are "inverted", since we have natural-scroll enabled for
      # touchpads by default.
      # Mod+TouchpadScrollDown { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.02+"; }
      # Mod+TouchpadScrollUp   { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.02-"; }

      # You can refer to workspaces by index. However, keep in mind that
      # niri is a dynamic workspace system, so these commands are kind of
      # "best effort". Trying to refer to a workspace index bigger than
      # the current workspace count will instead refer to the bottommost
      # (empty) workspace.
      #
      # For example, with 2 workspaces + 1 empty, indices 3, 4, 5 and so on
      # will all refer to the 3rd workspace.
      "Mod+1".action.focus-workspace = "1-browsing";
      "Mod+2".action.focus-workspace = "2-working";
      "Mod+3".action.focus-workspace = "3-planning";
      "Mod+4".action.focus-workspace = "4-communicating";
      "Mod+5".action.focus-workspace = "5-gaming";
      "Mod+6".action.focus-workspace = 6;
      "Mod+7".action.focus-workspace = 7;
      "Mod+8".action.focus-workspace = 8;
      "Mod+9".action.focus-workspace = 9;
      "Mod+0".action.focus-workspace = 10;
      "Mod+Ctrl+1".action.move-column-to-workspace = "1-browsing";
      "Mod+Ctrl+2".action.move-column-to-workspace = "2-working";
      "Mod+Ctrl+3".action.move-column-to-workspace = "3-planning";
      "Mod+Ctrl+4".action.move-column-to-workspace = "4-communicating";
      "Mod+Ctrl+5".action.move-column-to-workspace = "5-gaming";
      "Mod+Ctrl+6".action.move-column-to-workspace = 6;
      "Mod+Ctrl+7".action.move-column-to-workspace = 7;
      "Mod+Ctrl+8".action.move-column-to-workspace = 8;
      "Mod+Ctrl+9".action.move-column-to-workspace = 9;
      "Mod+Ctrl+0".action.move-column-to-workspace = 10;

      # Alternatively, there are commands to move just a single window:
      # Mod+Ctrl+1 { move-window-to-workspace 1; }

      # Switches focus between the current and the previous workspace.
      # Mod+Tab { focus-workspace-previous; }

      "Mod+Comma".action.consume-window-into-column = { };
      "Mod+Period".action.expel-window-from-column = { };

      # There are also commands that consume or expel a single window to the side.
      # Mod+BracketLeft  { consume-or-expel-window-left; }
      # Mod+BracketRight { consume-or-expel-window-right; }

      "Mod+R".action.switch-preset-column-width = { };
      "Mod+Shift+R".action.reset-window-height = { };
      "Mod+F".action.maximize-column = { };
      "Mod+Shift+F".action.fullscreen-window = { };
      "Mod+C".action.center-column = { };

      # Finer width adjustments.
      # This command can also:
      # * set width in pixels: "1000"
      # * adjust width in pixels: "-5" or "+5"
      # * set width as a percentage of screen width: "25%"
      # * adjust width as a percentage of screen width: "-10%" or "+10%"
      # Pixel sizes use logical, or scaled, pixels. I.e. on an output with scale 2.0,
      # set-column-width "100" will make the column occupy 200 physical screen pixels.
      "Mod+Minus".action.set-column-width = "-10%";
      "Mod+Equal".action.set-column-width = "+10%";

      # Finer height adjustments when in column with other windows.
      "Mod+Shift+Minus".action.set-window-height = "-10%";
      "Mod+Shift+Equal".action.set-window-height = "+10%";

      # Actions to switch layouts.
      # Note: if you uncomment these, make sure you do NOT have
      # a matching layout switch hotkey configured in xkb options above.
      # Having both at once on the same hotkey will break the switching,
      # since it will switch twice upon pressing the hotkey (once by xkb, once by niri).
      # Mod+Space       { switch-layout "next"; }
      # Mod+Shift+Space { switch-layout "prev"; }

      "Print".action.screenshot = { };
      "Ctrl+Print".action.screenshot-screen = { };
      "Alt+Print".action.screenshot-window = { };

      # The quit action will show a confirmation dialog to avoid accidental exits.
      "Mod+Shift+Q".action.quit = { };

      # Powers off the monitors. To turn them back on, do any input like
      # moving the mouse or pressing any other key.
      "Mod+Shift+P".action.power-off-monitors = { };
    };

    prefer-no-csd = true;

    window-rules = [
      {
        geometry-corner-radius = {
          bottom-left = 10.0;
          bottom-right = 10.0;
          top-left = 10.0;
          top-right = 10.0;
        };

        clip-to-geometry = true;
      }
    ];

    layout = {
      shadow = {
        enable = true;
      };
    };

    workspaces = {
      "1-browsing" = { };
      "2-working" = { };
      "3-planning" = { };
      "4-communicating" = { };
      "5-gaming" = { };
    };

    spawn-at-startup = [
      {
        command = [
          (lib.getExe pkgs.xwayland-satellite)
        ];
      }
    ];

    environment = {
      DISPLAY = ":0";
    };

    # window-rule {
    #     match at-startup=true app-id=r#"^org\.gnome\.Fractal$"#
    #     open-on-workspace "chat"
    # }
  };

  systemd.user.services.random-wallpaper = {
    Unit = {
      Description = "Randomly choose a wallpaper in ~/Pictures/Wallpapers";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = pkgs.writeShellScript "random-wallpaper" ''
        ${lib.getExe pkgs.swaybg} -m fill -i $(${random-wallpaper})
      '';
    };
  };

  programs.waybar.systemd.enable = true;

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
      # {
      #   timeout = 600;
      #   command = "${swaymsg} 'output * power off'";
      #   resumeCommand = "${swaymsg} 'output * power on'";
      # }
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

  # programs.gnome-pomodoro-swayidle = (
  #   lib.fix (self: {
  #     onstart = [
  #       "${swaymsg} 'gaps inner all set 0, bar mode hide'"
  #       "${sound} message-new-instant"
  #     ];

  #     onend = [ "${swaymsg} 'gaps inner all set 10, bar mode dock'" ];
  #     onresume = self.onstart;
  #     onpause = self.onend;
  #   })
  # );

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
    # wrap-scale-off
  ];

  xdg = {
    autostart.enable = false;
    mimeApps.defaultApplications = {
      "inode/directory" = "emacsclient.desktop";
    };
  };
}
