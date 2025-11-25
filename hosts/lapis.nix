{
  imports = [
    ../environments/dev.nix
    ../environments/gaming.nix
    ../environments/gui/sway.nix
    ../services/valent.nix
    ../users/kira.nix
  ];

  system.stateVersion = "24.05";

  users.defaultUser = "kira";
}
