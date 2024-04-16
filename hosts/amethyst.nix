{
  imports = [
    ../environments/default.nix
    ../users/kira.nix
  ];

  system.stateVersion = "23.11";

  users.defaultUser = "kira";
}
