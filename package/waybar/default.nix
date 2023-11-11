{ pkgs, ... }:

let
  waybar = pkgs.waybar;
in
{
  programs.waybar = {
    enable = true;
    package = waybar;
    settings = {
      mainBar = {
        ipc = true;
        layer = "top";
        position = "bottom";
        height = 32;
        modules-left = [ "sway/workspaces" "sway/mode" ];
        modules-center = [ "cpu" "memory" "disk" "temperature" "network" ];
        modules-right = [ "idle_inhibitor" "backlight" "battery" "wireplumber" "clock" "tray" ];

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
        };

        "temperature" = {
          critical-threshold = 80;
          format = "{temperatureC}°C {icon}";
          format-icons = [ "" "" "" "" "" ];
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
          format-icons = [ "" "" "" "" "" ];
        };

        "network" = {
          format-wifi = "{essid} ({signalStrength}%) ";
          format-ethernet = "{ifname} = {ipaddr}/{cidr} ";
          format-disconnected = "Disconnected ⚠";
        };

        "wireplumber" = {
          format = "{volume}% {icon}";
          format-muted = "";
          format-icons = [ "" "" "" ];
        };
      };
    };

    style = ./style.css;
  };

  home.packages = with pkgs; [
    font-awesome_6
  ];

  wayland.windowManager.sway = {
    config.bars = [{ command = "${waybar}/bin/waybar"; }];
    extraConfig = ''
      layer_effects 'waybar' 'blur enable; shadows enable'
    '';
  };
}
