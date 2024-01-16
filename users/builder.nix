{ config, pkgs, ... }:

{
  users = {
    users.builder = {
      isSystemUser = true;
      useDefaultShell = true;
      group = "builder";
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDG2wKEtIS2KuoXr4uNAzwTLvkyjrLX9zonE3pZB2pdH"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPBPD66/axLTeJHQ+lLmOSJT2VQyESnk5VRr7Rkx4BET"
      ];
    };

    groups.builder = { };
  };

  nix.settings.trusted-users = [ "builder" ];
}
