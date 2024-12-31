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

  programs.ssh.extraConfig = ''
    IdentityFile ${(builtins.head config.services.openssh.hostKeys).path}
  '';
}
