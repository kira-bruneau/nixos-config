{
  imports = [
    ../environments/default.nix
    ../services/minecraft/AboveAndBeyond.nix
    ../users/kira.nix
  ];

  system.stateVersion = "23.11";

  users.defaultUser = "kira";
}
