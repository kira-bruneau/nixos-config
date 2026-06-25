{ config, lib, ... }:

{
  networking.hosts."100.64.0.13" = lib.mkIf config.services.tailscale.enable [ "obsidian" ];
}
