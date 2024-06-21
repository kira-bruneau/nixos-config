{
  imports = [
    ../environments/gui/gnome.nix
    ../users/kira.nix
  ];

  system.stateVersion = "24.05";

  users.defaultUser = "kira";

  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
  };
}
