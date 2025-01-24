{
  imports = [
    ../environments/gui/sway.nix
    ../users/kira.nix
  ];

  system.stateVersion = "24.05";

  users.defaultUser = "kira";
}
