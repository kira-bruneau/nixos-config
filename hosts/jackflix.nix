{ lib, ... }:

{
  imports = [
    ../environments/autologin.nix
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
    ../users/jakira.nix
  ];

  system.stateVersion = "24.05";

  users.defaultUser = "jakira";

  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    xkb = {
      layout = lib.mkForce "us,us";
      variant = lib.mkForce ",colemak";
    };
  };
}
