{
  config,
  lib,
  pkgsCustom,
  ...
}:

{
  imports = [
    ../../home/environments/art.nix
    ../../home/environments/bluetooth.nix
    ../../home/environments/dev
    ../../home/environments/gaming.nix
    ../../home/environments/gui/sway.nix
    ../../home/environments/office.nix
    ../../home/programs/tsukimi
  ];

  home.stateVersion = "24.05";

  programs.waybar.settings.mainBar.temperature = {
    hwmon-path-abs = "/sys/devices/platform/asus-ec-sensors/hwmon";
    input-filename = "temp2_input";
  };

  systemd.user = {
    services = {
      update-nixos = {
        Service = {
          Type = "oneshot";
          WorkingDirectory = "${config.home.homeDirectory}/Dev/public/nixos-config/update";
          ExecStart = lib.getExe pkgsCustom.update.nixos;
        };
      };

      update-nixos-main = {
        Unit = {
          Requires = [ "update-nixos.service" ];
          After = [ "update-nixos.service" ];
        };

        Service = {
          Type = "oneshot";
          WorkingDirectory = "${config.home.homeDirectory}/Dev/public/nixos-config/main";
          ExecStart = lib.getExe pkgsCustom.update.nixos.main;
        };
      };

      update-nixos-unstable = {
        Unit = {
          Requires = [ "update-nixos.service" ];
          After = [ "update-nixos.service" ];
        };

        Service = {
          Type = "oneshot";
          WorkingDirectory = "${config.home.homeDirectory}/Dev/public/nixos-config/unstable";
          ExecStart = lib.getExe pkgsCustom.update.nixos.unstable;
        };
      };

      update-nixos-peridot = {
        Unit = {
          Requires = [ "update-nixos-unstable.service" ];
          After = [ "update-nixos-unstable.service" ];
        };

        Service = {
          Type = "oneshot";
          WorkingDirectory = "${config.home.homeDirectory}/Dev/public/nixos-config/peridot";
          ExecStart = lib.getExe pkgsCustom.update.nixos.peridot;
        };
      };

      update-nur = {
        Service = {
          Type = "oneshot";
          WorkingDirectory = "${config.home.homeDirectory}/Dev/public/nur-packages/main";
          ExecStart = lib.getExe pkgsCustom.update.nixos.nur;
        };
      };
    };

    timers = {
      update-nixos-main = {
        Timer = {
          OnCalendar = "weekly";
          Persistent = true;
        };

        Install.WantedBy = [ "timers.target" ];
      };

      update-nixos-peridot = {
        Timer = {
          OnCalendar = "weekly";
          Persistent = true;
        };

        Install.WantedBy = [ "timers.target" ];
      };

      update-nur = {
        Timer = {
          OnCalendar = [
            "*-*-* 00:00:00"
            "*-*-* 12:00:00"
          ];

          Persistent = true;
        };

        Install.WantedBy = [ "timers.target" ];
      };
    };
  };
}
