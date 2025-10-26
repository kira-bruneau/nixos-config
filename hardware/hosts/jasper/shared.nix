{ config, lib, ... }:

{
  networking.hosts."10.100.0.15" = lib.mkIf config.services.tailscale.enable [ "jasper" ];
}
