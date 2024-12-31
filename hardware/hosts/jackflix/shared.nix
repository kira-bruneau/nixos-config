{
  nix.settings.trusted-public-keys = [ "jackflix:CtqkVpen2v2aTo4xs8LsmvZcPeke0ewYAtjLRbh+Vvw=" ];
  networking.hosts."100.64.0.5" = [ "jackflix" ];
  programs.ssh.knownHosts.jackflix.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICHPbtzZIHQgeSmhPyuB7yUHCBJ+lt3aeiu7Ey4GK5ZB";
}
