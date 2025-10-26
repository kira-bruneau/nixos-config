{ config, lib, ... }:

{
  networking.hosts."10.100.0.11" = lib.mkIf config.services.tailscale.enable [ "azurite" ];
}
