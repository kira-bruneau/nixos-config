{ inputs, lib, ... }:

{
  imports = [
    inputs.home-manager.nixosModules.default
    ../cachix.nix
    ../services/dnscrypt.nix
    ./custom-nixos-generate-config.nix
    ./locale
    ./nix.nix
  ];

  environment.defaultPackages = [];

  users.mutableUsers = false;

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    sharedModules = [ ../home/environments/default.nix ];
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
    "anytype"
    "anytype-heart"
    "clonehero"
    "clonehero-unwrapped"
    "data.zip"
    "discord"
    "minecraft-server"
    "sm64ex"
    "steam"
    "steam-jupiter-original"
    "steam-original"
    "steam-run"
    "steamdeck-hw-theme"
    "unrar"
    "vvvvvv"
  ];
}
