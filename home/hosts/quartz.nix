{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ../environments/art.nix
    ../environments/bluetooth.nix
    ../environments/dev
    ../environments/gaming.nix
    ../environments/gui/sway.nix
    ../environments/office.nix
  ];

  home.stateVersion = "24.05";

  programs.waybar.settings.mainBar.temperature = {
    hwmon-path-abs = "/sys/devices/platform/asus-ec-sensors/hwmon";
    input-filename = "temp2_input";
  };

  systemd.user = {
    services = {
      update-nixos-main = {
        Unit = {
          Wants = [ "update-nur.service" ];
          After = [ "update-nur.service" ];
        };

        Service = {
          Type = "oneshot";
          WorkingDirectory = "${config.home.homeDirectory}/Dev/public/nixos-config/main";
          ExecStart = pkgs.writeShellScript "update-nixos" ''
            export PATH=${
              lib.makeBinPath (
                with pkgs;
                [
                  nix
                  nixos-rebuild
                  git
                ]
              )
            }

            nix flake update --commit-lock-file --option commit-lockfile-summary "flake.lock: update"
            nixos-rebuild build --flake .
          '';
        };
      };

      update-nur = {
        Service = {
          Type = "oneshot";
          WorkingDirectory = "${config.home.homeDirectory}/Dev/public/nur-packages";
          ExecStart = pkgs.writeShellScript "update-nur" ''
            export PATH=${lib.makeBinPath (with pkgs; [ nix ])}
            nix run .#update
            nix run .#sync ~/Dev/nixpkgs/nur
          '';
        };
      };
    };

    timers.update-nixos-main = {
      Timer.OnCalendar = "daily";
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
