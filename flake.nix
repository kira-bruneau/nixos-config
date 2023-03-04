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

  outputs = { nixpkgs, my-nur, nixos-generators, ... } @ inputs:
    let
      lib = nixpkgs.lib;

      hostsDir = ./host;

      hosts = builtins.listToAttrs
        (builtins.concatMap
          (host:
            if lib.hasSuffix ".nix" host
            then [
              {
                name = lib.removeSuffix ".nix" host;
                value = /${hostsDir}/${host};
              }
            ]
            else [ ])
          (builtins.attrNames (builtins.readDir hostsDir)));

      commonModules = [
        ./cachix.nix
        ./environment/seed-nixos-config.nix

        # Overlay my nur packages & modules
        my-nur.nixosModules.overlay

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
      ];
    in
    {
      nixosConfigurations = builtins.mapAttrs
        (host: path:
          lib.nixosSystem {
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
            }).nixpkgs.hostPlatform;
          in
            packages // {
              ${system} = (packages.${system} or {}) // {
                "${host}-install-iso" = nixos-generators.nixosGenerate {
                  pkgs = nixpkgs.legacyPackages.${system};
                  format = "install-iso";
                  specialArgs = { inherit inputs; };
                  modules = commonModules ++ [
                    {
                      # /etc/nixos is seeded with the contents of this flake
                      installer.cloneConfig = false;

                      # Disable ZFS support, it may not be compatible
                      # with the configured kernel version
                      boot.supportedFilesystems = lib.mkForce
                        [ "btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs" ];

                      networking = {
                        hostName = host;

                        # I prefer manual wpa_supplicant configuration
                        wireless.userControlled.enable = lib.mkForce false;
                      };

                      # Resolve conflict between install iso config and my host configs
                      services.openssh.permitRootLogin = lib.mkForce "no";
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
