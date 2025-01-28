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
          ExecStart = pkgs.writeShellScript "update-nixos-main" ''
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

      update-nixos-unstable = {
        Unit = {
          Wants = [ "update-nixos-main.service" ];
          After = [ "update-nixos-main.service" ];
        };

        Service = {
          Type = "oneshot";
          WorkingDirectory = "${config.home.homeDirectory}/Dev/public/nixos-config/unstable";
          ExecStart = pkgs.writeShellScript "update-nixos-unstable" ''
            export PATH=${
              lib.makeBinPath (
                with pkgs;
                [
                  nix
                  git
                ]
              )
            }

            if ! git pull --rebase; then
              rm flake.lock
              nix flake lock
              git add flake.lock
              GIT_EDITOR=true git rebase --continue
            fi
          '';
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
          ExecStart = pkgs.writeShellScript "update-nixos-peridot" ''
            export PATH=${
              lib.makeBinPath (
                with pkgs;
                [
                  git
                  nix
                  nixos-rebuild
                ]
              )
            }

            git pull --rebase
            nixos-rebuild build --flake .#peridot
          '';
        };
      };

      update-nur = {
        Service = {
          Type = "oneshot";
          WorkingDirectory = "${config.home.homeDirectory}/Dev/public/nur-packages";
          ExecStart = pkgs.writeShellScript "update-nur" ''
            export PATH=${lib.makeBinPath (with pkgs; [ nix ])}
            nix run .#sync ~/Dev/nixpkgs/nur
          '';
        };
      };
    };

    timers = {
      update-nixos-main = {
        Timer.OnCalendar = "daily";
        Install.WantedBy = [ "timers.target" ];
      };

      update-nixos-peridot = {
        Timer.OnCalendar = "weekly";
        Install.WantedBy = [ "timers.target" ];
      };
    };
  };
}
