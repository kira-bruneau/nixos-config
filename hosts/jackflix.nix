{ inputs, lib, ... }:

{
  imports = [
    ../environments/autologin.nix
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
    ../users/jakira.nix
  ];

  system.stateVersion = "23.11";

  users.defaultUser = "jakira";

  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
  };
}
