{
  imports = [
    ../environments/autologin.nix
    ../environments/dev.nix
    ../environments/gui/sway.nix
    ../users/kira.nix
  ];

  system.stateVersion = "25.05";

  users.defaultUser = "kira";
}
