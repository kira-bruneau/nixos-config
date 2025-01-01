{
  imports = [
    ../users/builder.nix
    ../users/kira.nix
  ];

  system.stateVersion = "24.05";

  users.defaultUser = "kira";
}
