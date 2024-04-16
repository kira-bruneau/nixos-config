{
  imports = [
    ../environments/default.nix
    ../users/builder.nix
    ../users/kira.nix
  ];

  system.stateVersion = "23.11";

  users.defaultUser = "kira";
}
