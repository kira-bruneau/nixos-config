{ config, lib, ... }:

{
  nix.settings.trusted-public-keys = [ "steamdeck:BcQXU+d7+azmiE/6YBWs/OJpIYlhcuTcpTU2j7+Zxb8=" ];
  networking.hosts."100.64.0.3" = lib.mkIf config.services.tailscale.enable [ "peridot" ];
  programs.ssh.knownHosts.peridot.publicKey =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO4pl3ks7YENIrN5/K/QYRxehqTeHHTOHCPPnQ/7kHVL";
}
