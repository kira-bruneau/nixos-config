{ ... }:

{
  programs.waybar = {
    enable = true;

    settings = {
      mainBar = {
        ipc = true;
        layer = "top";
        position = "bottom";
        height = 32;
        modules-left = [ "sway/workspaces" "sway/mode" ];
        modules-center = [ "cpu" "memory" "disk" "temperature" "network" ];
        modules-right = [ "idle_inhibitor" "backlight" "battery" "pulseaudio" "clock" "tray" ];

        "sway/workspaces" = {
          all-outputs = true;
          format = "{icon}";
          format-icons = {
            "1" = "";
            "2" = "";
            "3" = "";
            "4" = "";
            "5" = "";
            "7" = "";
            "8" = "";
            "9" = "";
            "10" = "";
            urgent = "";
            focused = "";
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
          format = "{}% ";
        };

        "temperature" = {
          critical-threshold = 80;
          format = "{temperatureC}°C {icon}";
          format-icons = [ "" "" "" "" "" ];
        };

        "backlight" = {
          format = "{percent}% {icon}";
          format-icons = [ "" "" ];
        };

        "battery" = {
          states = {
            warning = 30;
            critical = 10;
          };
          format = "{capacity}% {icon}";
          format-icons = [ "" "" "" "" "" ];
        };

        "network" = {
          format-wifi = "{essid} ({signalStrength}%) ";
          format-ethernet = "{ifname} = {ipaddr}/{cidr} ";
          format-disconnected = "Disconnected ⚠";
        };

        "pulseaudio" = {
          format = "{volume}% {icon}";
          format-bluetooth = "{volume}% {icon}";
          format-muted = "";
          format-icons = {
            headphones = "";
            handsfree = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = [ "" "" ];
          };
          on-click = "pavucontrol";
        };
      };
    };

    style = ./style.css;
  };
}
