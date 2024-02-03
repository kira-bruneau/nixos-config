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
      "/srv" = {
        hostPath = "/srv";
        isReadOnly = false;
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

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "steam"
    "steam-original"
    "steam-run"
  ];
}
