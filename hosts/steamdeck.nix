{ inputs, lib, ... }:

{
  imports = [
    ../environments/gaming.nix
    ../environments/gui/gnome.nix
    ../users/kira.nix
    inputs.jovian.nixosModules.default
  ];

  system.stateVersion = "23.11";

  jovian.steam = {
    enable = true;
    autoStart = true;
    user = "kira";
    desktopSession = "gnome";
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "steam"
    "steam-jupiter-original"
    "steam-run"
    "steamdeck-hw-theme"
  ];
}
