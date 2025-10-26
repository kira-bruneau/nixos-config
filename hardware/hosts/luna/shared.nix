{ config, lib, ... }:

{
  networking.hosts."10.100.0.4" = lib.mkIf config.services.tailscale.enable [ "luna" ];

  services.syncthing.settings = {
    devices.luna.id = "O4NQTDT-NWV3GEZ-67BW33I-BQ454SI-42G2RK3-F53W4L4-RUG47VK-5VXLFA7";

    folders = {
      "Auth".devices = [ "luna" ];
    };
  };
}
