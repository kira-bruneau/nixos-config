{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    my-nur = {
      url = "gitlab:kira-bruneau/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:kira-bruneau/home-manager/firefox-search";
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

  outputs = { nixpkgs, my-nur, nixos-generators, ... } @ inputs:
    let
      hostsDir = ./host;
      hosts = builtins.listToAttrs
        (builtins.concatMap
          (host:
            if nixpkgs.lib.hasSuffix ".nix" host
            then [
              {
                name = nixpkgs.lib.removeSuffix ".nix" host;
                value = hostsDir + "/${host}";
              }
            ]
            else [ ])
          (builtins.attrNames (builtins.readDir hostsDir)));

      commonModules = [
        ./cachix.nix

        {
          nix = {
            settings = {
              auto-optimise-store = true;
              experimental-features = [ "nix-command" "flakes" ];
            };

            # Pin nixpkgs in flake registry
            registry.nixpkgs.flake = nixpkgs;

            # Pin nixpkgs channel (for backwards compatibility with nix2 cli)
            nixPath = [ "nixpkgs=${nixpkgs}" ];
          };
        }

        # Overlay my nur packages & modules
        {
          nixpkgs.overlays = [ my-nur.overlays.default ];
          disabledModules = [
            "hardware/xpadneo.nix"
            "programs/bash/undistract-me.nix"
            "programs/gamemode.nix"
            "services/video/replay-sorcery.nix"
          ];
        }
      ] ++ builtins.attrValues my-nur.nixosModules;
    in
    {
      nixosConfigurations = builtins.mapAttrs
        (host: path:
          nixpkgs.lib.nixosSystem {
            specialArgs = { inherit inputs; };
            modules = commonModules ++ [
              {
                networking.hostName = host;
              }
              path
            ];
          })
        hosts;

      packages = builtins.foldl'
        (packages: host:
          let
            path = hosts.${host};
            system = (import path {
              inputs = null;
              pkgs = null;
            }).nixpkgs.hostPlatform.system;
          in
            packages // {
              ${system} = (packages.${system} or {}) // {
                "${host}-install-iso" = nixos-generators.nixosGenerate {
                  pkgs = nixpkgs.legacyPackages.${system};
                  format = "install-iso";
                  specialArgs = { inherit inputs; };
                  modules = commonModules ++ [
                    {
                      networking.hostName = host;
                      installer.cloneConfig = false;
                      services.openssh.permitRootLogin = nixpkgs.lib.mkForce "no";
                    }
                    path
                  ];
                };
              };
            })
        {}
        (builtins.attrNames hosts);
    };
}
