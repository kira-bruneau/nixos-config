{ config, lib, ... }:

{
  networking.hosts."10.100.0.5" = lib.mkIf config.services.tailscale.enable [ "jackflix" ];
}
