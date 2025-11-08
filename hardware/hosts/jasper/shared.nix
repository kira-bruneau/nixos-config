{ config, lib, ... }:

{
  networking.hosts."100.64.0.15" = lib.mkIf config.services.tailscale.enable [ "jasper" ];
}
