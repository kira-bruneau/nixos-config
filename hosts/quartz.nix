{ config, lib, ... }:

{
  imports = [
    ../environments/gaming.nix
    ../environments/gui/sway.nix
    ../services/kubo.nix
    ../services/minecraft/BigChadGuysPlus.nix
    ../users/builder.nix
    ../users/kira.nix
  ];

  system.stateVersion = "22.11";

  containers.media-server = {
    autoStart = true;

    bindMounts = {
      "/srv/media-ssd" = { isReadOnly = false; };
      "/srv/media-hdd" = {};
    };

    config = {
      imports = [ ../environments/media-server.nix ];
      system.stateVersion = "22.11";
      fonts.fontconfig.enable = false;
    };
  };

  systemd.services."container@media-server".unitConfig.RequiresMountsFor =
    builtins.map
      (d: if d.hostPath != null then d.hostPath else d.mountPoint)
      (builtins.attrValues config.containers.media-server.bindMounts);

  networking.firewall.allowedTCPPorts = [
    8096
    5055
  ];
}
