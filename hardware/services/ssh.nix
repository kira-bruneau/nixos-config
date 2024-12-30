{ config, ... }:

{
  services.openssh = {
    enable = true;

    hostKeys = [
      {
        path = "/persist/etc/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
    ];

    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  programs.ssh = {
    knownHosts = {
      amethyst.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEt3gfHRXW9WP0/wff2WEVGqlr/6b6jtr6fpz9uRkotS";
      aurora.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDG2wKEtIS2KuoXr4uNAzwTLvkyjrLX9zonE3pZB2pdH";
      jackflix.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICHPbtzZIHQgeSmhPyuB7yUHCBJ+lt3aeiu7Ey4GK5ZB";
      quartz.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPBPD66/axLTeJHQ+lLmOSJT2VQyESnk5VRr7Rkx4BET";
      rocky.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINUGzRm127zLH3ZQxwGTFmWyggeu7uwoaMnCGUdCq0fO";
      steamdeck.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO4pl3ks7YENIrN5/K/QYRxehqTeHHTOHCPPnQ/7kHVL";
    };

    extraConfig = ''
      IdentityFile ${(builtins.head config.services.openssh.hostKeys).path}
    '';
  };
}
