{
  nix.settings.trusted-public-keys = [ "jakira:wNeIHrxXz2JonyeKJ8Dx6YThMAt1oTn58kVCOAYR1JI=" ];

  programs.ssh.knownHosts.jakira.publicKey =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKGA1ahkvVffvxr4l/tXLOxOAa4RnenIY5PQjx0D+BaO";
}
