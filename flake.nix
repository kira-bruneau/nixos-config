{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    kira-nur = {
      url = "gitlab:kira-bruneau/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    kira-home-config = {
      url = "gitlab:kira-bruneau/home-config";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        my-nur.follows = "kira-nur";
      };
    };
  };

  outputs = { nixpkgs, nixos-generators, ... } @ inputs:
    let
      lib = nixpkgs.lib;

      hostsDir = ./host;

      hosts = builtins.listToAttrs
        (builtins.map
          (host: {
            name = host;
            value = hostsDir + "/${host}";
          })
          (builtins.attrNames (builtins.readDir hostsDir)));

      commonModules = with inputs; [
        disko.nixosModules.disko
        kira-nur.nixosModules.overlay
        ./cachix.nix
        ./environment/nix.nix
        ./environment/custom-nixos-generate-config.nix
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
              (path + "/hardware.nix")
              path
            ];
          })
        hosts;

      packages = builtins.foldl'
        (packages: host:
          let
            path = hosts.${host};
            config = (import (path + "/hardware.nix") {
              pkgs = nixpkgs.legacyPackages.${system};
              modulesPath = nixpkgs + "/modules";
              inherit config lib;
            });

            system = config.nixpkgs.hostPlatform.content;
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

                      disko.enableConfig = false;

                      networking = {
                        hostName = host;

                        # I use iwd instead
                        wireless.enable = false;
                      };

                      # Resolve conflict between install iso config and my host configs
                      services.openssh.settings.PermitRootLogin = lib.mkForce "no";
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
