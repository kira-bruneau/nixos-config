{ config, lib, ... }:

{
  imports = [
    ../../environments/gui/gnome.nix
    ../../users/kira.nix
  ];

  system.stateVersion = config.system.nixos.release;

  home-manager.sharedModules = [
    {
      home.stateVersion = config.system.nixos.release;
    }
  ];

  users.defaultUser = "kira";

  services.displayManager.gdm.enable = true;
}
