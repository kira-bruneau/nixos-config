{
  nix.settings.trusted-public-keys = [ "aurora:PkeJpeCTFE3gprtNpxCW0EqbVwg0wFgvpHFq3Hj0Wlc=" ];

  networking.hosts."100.64.0.2" = [ "aurora" ];

  programs.ssh.knownHosts.aurora.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDG2wKEtIS2KuoXr4uNAzwTLvkyjrLX9zonE3pZB2pdH";

  services.syncthing.settings = {
    devices.aurora.id = "ODCDVEV-I63ZAW6-MV27YBB-W5MDOAU-YZ3RK23-DMXCWAN-STJOSEF-EFXFRQP";

    folders = {
      "Auth".devices = [ "aurora" ];
      "Dev".devices = [ "aurora" ];
      "Documents".devices = [ "aurora" ];
      "Pictures".devices = [ "aurora" ];
      "RSS".devices = [ "aurora" ];
      "Videos".devices = [ "aurora" ];
    };
  };
}
