{ lib, ... }:

{
  imports = [
    ../environment/cli.nix
    ../environment/gui.nix
  ];

  services.syncthing.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "slack"
    "unrar"
  ];

  home.stateVersion = "21.11";
}
