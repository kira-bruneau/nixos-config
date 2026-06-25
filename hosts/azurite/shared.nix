{ config, lib, ... }:

{
  networking.hosts."100.64.0.11" = lib.mkIf config.services.tailscale.enable [ "azurite" ];
}
