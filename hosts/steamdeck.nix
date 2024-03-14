{ inputs, lib, ... }:

{
  imports = [
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
    ../users/kira.nix
    inputs.jovian.nixosModules.default
  ];

  system.stateVersion = "23.11";

  jovian = {
    steam = {
      enable = true;
      autoStart = true;
      user = "kira";
      desktopSession = "gnome";
    };

    decky-loader.enable = true;
  };
}
