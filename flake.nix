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
      url = "gitlab:kira-bruneau/home-config";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        my-nur.follows = "my-nur";
      };
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, my-nur, home-manager, home-config-kira, nixos-hardware, nixos-generators }:
    let
      common-modules = host: [
        {
          nix = {
            # Pin nixpkgs flake to the one used to build this config
            registry.nixpkgs.flake = nixpkgs;
            nixPath = [ "nixpkgs=${nixpkgs}" ];
          };
        }
        {
          # Add my NUR overlay
          nixpkgs.overlays = [ my-nur.overlays.default ];
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
            users.kira = home-config-kira.nixosModules.${host};
          };
        }
        ./cachix.nix
      ];

      atlantis-modules = (common-modules "atlantis") ++ (with nixos-hardware.nixosModules; [
        common-cpu-amd
        common-gpu-amd
        common-pc-ssd
        ./host/atlantis.nix
      ]);

      framework-modules = (common-modules "framework") ++ (with nixos-hardware.nixosModules; [
        framework
        ./host/framework.nix
      ]);
    in
    {
      nixosConfigurations = {
        atlantis = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs.self = self;
          modules = atlantis-modules;
        };
        framework = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs.self = self;
          modules = framework-modules;
        };
      };

      packages.x86_64-linux = {
        framework-iso = nixos-generators.nixosGenerate {
          format = "install-iso";
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          specialArgs.self = self;
          modules = framework-modules ++ [
            {
              installer.cloneConfig = false;
            }
          ];
        };
      };
    };
}
