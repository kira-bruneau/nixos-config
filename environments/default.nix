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

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
  };

  nixpkgs.config = {
    allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
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

    permittedInsecurePackages = [
      "nix-2.16.2" # Required by nixd
    ];
  };
}
