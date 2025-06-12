{ config, lib, ... }:

{
  nix.settings.trusted-public-keys = [ "sapphire:RJh+t5PSv5uR/LzUvTjhHagAikBQmrT3thHTOr23Olc=" ];

  networking.hosts."100.64.0.7" = lib.mkIf config.services.tailscale.enable [ "sapphire" ];

  programs.ssh.knownHosts.sapphire.publicKey =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ3dUswKJpYDitJJnikD3JrOUT8mNvsy5RJ+RSUm9yWE";
}
