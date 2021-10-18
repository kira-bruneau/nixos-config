{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    my-nur = {
      url = "gitlab:kira-bruneau/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-config-kira = {
      url = "/home/kira/Dev/home-config";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        my-nur.follows = "my-nur";
      };
    };
  };

  outputs = { self, nixpkgs, my-nur, home-manager, home-config-kira }: {
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
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useUserPackages = true;
              users.kira = home-config-kira.nixosModules.atlantis;
            };
          }
          ./cachix.nix
          ./host/atlantis.nix
        ];
      };
    };
  };
}
