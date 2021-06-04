{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    my-nur = {
      url = "github:kira-bruneau/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, my-nur }: {
    nixosConfigurations = {
      atlantis = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          {
            # Pin nixpkgs flake to the one used to build this config
            nix.registry.nixpkgs.flake = nixpkgs;
          }
          {
            # Add my NUR overlay
            nixpkgs.overlays = [ my-nur.overlay ];
            disabledModules = [
              "hardware/xpadneo.nix"
              "programs/bash/undistract-me.nix"
              "programs/gamemode.nix"
              "services/video/replay-sorcery.nix"
            ];
          }
          my-nur.nixosModules.gamemode
          my-nur.nixosModules.replay-sorcery
          my-nur.nixosModules.undistract-me
          my-nur.nixosModules.xpadneo
          ./cachix.nix
          ./host/atlantis.nix
        ];
      };
    };
  };
}
