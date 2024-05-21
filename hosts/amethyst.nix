{
  imports = [
    ../environments/default.nix
    ../services/minecraft/AboveAndBeyond.nix
    ../services/minecraft/BigChadGuysPlus.nix
    ../users/kira.nix
  ];

  system.stateVersion = "23.11";

  users.defaultUser = "kira";
}
