{
  imports = [
    ../environments/default.nix
    ../services/minecraft/Jakira
    ../users/kira.nix
  ];

  system.stateVersion = "24.05";

  users.defaultUser = "kira";
}
