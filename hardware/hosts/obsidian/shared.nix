{ config, lib, ... }:

{
  networking.hosts."10.100.0.13" = lib.mkIf config.services.tailscale.enable [ "obsidian" ];
}
