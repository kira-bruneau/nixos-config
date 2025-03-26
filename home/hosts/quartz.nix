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
      update-nixos = {
        Service = {
          Type = "oneshot";
          WorkingDirectory = "${config.home.homeDirectory}/Dev/public/nixos-config/update";
          ExecStart = lib.getExe (
            pkgs.writeShellApplication {
              name = "update-nixos";

              runtimeInputs = with pkgs; [
                git
                nix
                nixos-rebuild
              ];

              text = ''
                cleanup() {
                    local exit_code changes
                    exit_code=$?
                    changes="$(git rev-parse HEAD)"
                    git reset --hard '@{-1}'
                    git switch -
                    if [ "$exit_code" -eq 0 ]; then
                        git merge --ff-only "$changes"
                    fi
                }

                git switch --detach HEAD
                trap cleanup EXIT
                nix flake update --commit-lock-file --option commit-lockfile-summary "flake.lock: update"
                nixos-rebuild build --flake .
              '';
            }
          );
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
          ExecStart = lib.getExe (
            pkgs.writeShellApplication {
              name = "update-nixos-main";

              runtimeInputs = with pkgs; [
                git
                nix
                nixos-rebuild
              ];

              text = ''
                git rebase update
                nixos-rebuild build --flake .
              '';
            }
          );
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
          ExecStart = lib.getExe (
            pkgs.writeShellApplication {
              name = "update-nixos-unstable";

              runtimeInputs = with pkgs; [
                coreutils
                git
                nix
              ];

              text = ''
                if ! git rebase update; then
                  rm flake.lock
                  nix flake lock
                  git add flake.lock
                  GIT_EDITOR=true git rebase --continue
                fi
              '';
            }
          );
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
          ExecStart = lib.getExe (
            pkgs.writeShellApplication {
              name = "update-nixos-peridot";
              text = ''
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

                git rebase unstable
                nixos-rebuild build --flake .#peridot
              '';
            }
          );
        };
      };

      update-nur = {
        Service = {
          Type = "oneshot";
          WorkingDirectory = "${config.home.homeDirectory}/Dev/public/nur-packages/main";
          ExecStart = lib.getExe (
            pkgs.writeShellApplication {
              name = "update-nur";
              runtimeInputs = with pkgs; [ nix ];
              text = ''
                nix run .#sync ~/Dev/nixpkgs/nur
              '';
            }
          );
        };
      };
    };

    timers = {
      update-nixos-main = {
        Timer = {
          OnCalendar = [
            "*-*-* 00:00:00"
            "*-*-* 12:00:00"
          ];

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
          OnCalendar = "daily";
          Persistent = true;
        };

        Install.WantedBy = [ "timers.target" ];
      };
    };
  };
}
