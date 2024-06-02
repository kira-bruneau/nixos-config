{
  users = {
    users.builder = {
      isSystemUser = true;
      useDefaultShell = true;
      group = "builder";
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICHPbtzZIHQgeSmhPyuB7yUHCBJ+lt3aeiu7Ey4GK5ZB"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDG2wKEtIS2KuoXr4uNAzwTLvkyjrLX9zonE3pZB2pdH"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEt3gfHRXW9WP0/wff2WEVGqlr/6b6jtr6fpz9uRkotS"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO4pl3ks7YENIrN5/K/QYRxehqTeHHTOHCPPnQ/7kHVL"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPBPD66/axLTeJHQ+lLmOSJT2VQyESnk5VRr7Rkx4BET"
      ];
    };

    groups.builder = { };
  };

  nix.settings.keep-outputs = true;
}
