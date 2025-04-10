{ config, lib, ... }:

{
  nix.settings.trusted-public-keys = [ "aurora:PkeJpeCTFE3gprtNpxCW0EqbVwg0wFgvpHFq3Hj0Wlc=" ];

  networking.hosts."100.64.0.2" = lib.mkIf config.services.tailscale.enable [ "lapis" ];

  programs.ssh.knownHosts.lapis.publicKey =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDG2wKEtIS2KuoXr4uNAzwTLvkyjrLX9zonE3pZB2pdH";

  services.syncthing.settings = {
    devices.lapis.id = "ODCDVEV-I63ZAW6-MV27YBB-W5MDOAU-YZ3RK23-DMXCWAN-STJOSEF-EFXFRQP";

    folders = {
      "Auth".devices = [ "lapis" ];
      "Dev".devices = [ "lapis" ];
      "Documents".devices = [ "lapis" ];
      "Pictures".devices = [ "lapis" ];
      "RSS".devices = [ "lapis" ];
      "Videos".devices = [ "lapis" ];
    };
  };
}
