{
  config,
  lib,
  pkgs,
  ...
}:

let
  exe = lib.getExe config.programs.waybar.package;
in
{
  programs.waybar = {
    enable = true;

    package = pkgs.writeShellScriptBin "waybar" ''
      echo "" > ~/.config/waybar/dynamic.css
      exec ${lib.getExe pkgs.waybar}
    '';

    settings = {
      mainBar = {
        ipc = true;
        reload_style_on_change = true;

        layer = "top";
        position = "bottom";
        height = 32;

        modules-left = [
          "sway/workspaces"
          "sway/mode"
        ];

        modules-center = [
          "cpu"
          "memory"
          "disk"
          "temperature"
          "battery"
        ];

        modules-right = [
          "custom/audio_idle_inhibitor"
          "idle_inhibitor"
          "backlight"
          "wireplumber"
          "network"
          "clock"
          "tray"
        ];

        "sway/workspaces" = {
          all-outputs = true;
          format = "{icon}";
          format-icons = {
            "1" = ''<span font="Font Awesome 6 Brands"></span>'';
            "2" = "";
            "3" = "";
            "4" = "";
            "5" = "";
            "7" = "";
            "8" = "";
            "9" = "";
            "10" = "";
            default = "";
          };
        };

        "sway/mode" = {
          format = "<span style=\"italic\">{}</span>";
        };

        "idle_inhibitor" = {
          format = "{icon}";
          format-icons = {
            activated = "";
            deactivated = "";
          };
        };

        "tray" = {
          spacing = 10;
        };

        "clock" = {
          format = "{:%H:%M:%S}";
          format-alt = "{:%a %b %d, %Y}";
          interval = 1;
          tooltip-format = "{:%a %b %d, %Y | %H:%M:%S}";
        };

        "cpu" = {
          format = "{usage}% ";
          tooltip = false;
        };

        "memory" = {
          format = "{}% ";
        };

        "disk" = {
          format = "{percentage_used}% ";
          path = "/persist";
        };

        "temperature" = {
          critical-threshold = 80;
          format = "{temperatureC}°C {icon}";
          format-icons = [
            ""
            ""
            ""
            ""
            ""
          ];
        };

        "backlight" = {
          format = "{percent}% ";
        };

        "battery" = {
          states = {
            warning = 30;
            critical = 10;
          };
          format = "{capacity}% {icon}";
          format-icons = [
            ""
            ""
            ""
            ""
            ""
          ];
        };

        "network" = {
          format-wifi = "{signalStrength}% ";
          format-ethernet = "";
          format-disconnected = "⚠";
          tooltip-format-wifi = "{essid} ({signalStrength}%)";
          tooltip-format-ethernet = "{ifname} = {ipaddr}/{cidr}";
          tooltip-format-disconnected = "Disconnected ⚠";
        };

        "wireplumber" = {
          format = "{volume}% {icon}";
          format-muted = "";
          format-icons = [
            ""
            ""
            ""
          ];
        };

        "custom/audio_idle_inhibitor" = {
          format = "{icon}";
          exec = "${pkgs.sway-audio-idle-inhibit}/bin/sway-audio-idle-inhibit --dry-print-both-waybar";
          return-type = "json";
          format-icons = {
            output = "";
            input = "";
            output-input = "  ";
            none = "";
          };
        };
      };
    };

    style = ./style.css;
  };

  home.packages = with pkgs; [ font-awesome_6 ];

  wayland.windowManager.sway = {
    config.bars = [ { command = exe; } ];
    extraConfig = ''
      layer_effects 'waybar' 'blur enable; shadows enable'
    '';
  };

  programs.gnome-pomodoro-swayidle = {
    onpause = [
      "echo 'window#waybar { border-bottom-color: #ffa000; }' > ~/.config/waybar/dynamic.css"
    ];

    onresume = [ "echo '' > ~/.config/waybar/dynamic.css" ];
  };
}
