{ config, pkgs, ... }:

{
  fileSystems."/media/box-of-lies" = {
    device = "rock64:/media/box-of-lies";
    fsType = "nfs";
    options = [
      "noauto"
      "x-systemd.automount"
      "x-systemd.requires=network-online.target"
      "x-systemd.device-timeout=10"
      "x-systemd.idle-timeout=1min"
      "timeo=10"
    ];
  };
}
