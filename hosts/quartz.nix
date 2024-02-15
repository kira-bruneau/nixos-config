{ lib, ... }:

{
  imports = [
    ../environments/gaming.nix
    ../environments/gui/sway.nix
    ../services/kubo.nix
    ../users/builder.nix
    ../users/kira.nix
  ];

  system.stateVersion = "22.11";

  containers.media-server = {
    autoStart = true;

    bindMounts = {
      "/srv/media-ssd" = {
        hostPath = "/srv/media-ssd";
        isReadOnly = false;
      };
      "/srv/media-hdd" = {
        hostPath = "/srv/media-hdd";
      };
    };

    config = {
      imports = [ ../environments/media-server.nix ];
      system.stateVersion = "22.11";
      fonts.fontconfig.enable = false;
    };
  };

  networking.firewall.allowedTCPPorts = [
    8096
    5055
  ];

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
    "anytype"
    "anytype-heart"
    "clonehero"
    "clonehero-unwrapped"
    "data.zip"
    "discord"
    "sm64ex"
    "steam"
    "steam-original"
    "steam-run"
    "unrar"
    "vvvvvv"
  ];
}
