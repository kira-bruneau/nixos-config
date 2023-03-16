{ config, pkgs, ... }:

{
  imports = [
    ../service/ssh.nix
  ];

  system.activationScripts.seedBinaryCacheKey = ''
    if ! [ -f /etc/nix/private-key ]; then
      ${pkgs.nix}/bin/nix-store --generate-binary-cache-key \
        ${config.networking.hostName}-1 \
        /etc/nix/private-key \
        /etc/nix/public-key
    fi
  '';

  nix = {
    sshServe = {
      enable = true;
      protocol = "ssh-ng";
      keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDG2wKEtIS2KuoXr4uNAzwTLvkyjrLX9zonE3pZB2pdH"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPBPD66/axLTeJHQ+lLmOSJT2VQyESnk5VRr7Rkx4BET"
      ];
    };

    settings = {
      secret-key-files = "/etc/nix/private-key";
      trusted-users = [ "nix-ssh" ];
    };
  };
}
