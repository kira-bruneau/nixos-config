{
  description = "My NixOS configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    flake-linter = {
      url = "gitlab:kira-bruneau/flake-linter";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    kira-nur = {
      url = "gitlab:kira-bruneau/nur-packages";
      inputs = {
        flake-linter.follows = "flake-linter";
        flake-utils.follows = "flake-utils";
      };
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { flake-utils, flake-linter, nixpkgs, nixos-generators, ... } @ inputs:
    let
      lib = nixpkgs.lib;
      hosts = builtins.listToAttrs
        (builtins.map
          (file:
            let
              name = lib.removeSuffix ".nix" file;
            in
            {
              inherit name;
              value = {
                imports = [ ./hosts/${file} ];
                networking.hostName = name;
              };
            })
          (builtins.attrNames (builtins.readDir ./hosts)));
    in
    {
      nixosConfigurations = builtins.listToAttrs
        (builtins.concatMap
          (hostName:
            if builtins.pathExists ./hardware/hosts/${hostName}
            then [
              {
                name = hostName;
                value = lib.nixosSystem {
                  specialArgs = { inherit inputs; };
                  modules = [
                    ./hardware/hosts/${hostName}
                    hosts.${hostName}
                  ];
                };
              }
            ]
            else [ ]
          )
          (builtins.attrNames hosts));
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        flake-linter-lib = flake-linter.lib.${system};

        paths = flake-linter-lib.partitionToAttrs
          flake-linter-lib.commonPaths
          (builtins.filter
            (path:
              (builtins.all
                (ignore: !(nixpkgs.lib.hasSuffix ignore path))
                [
                  "cachix.nix"
                  "generated.nix"
                ]))
            (flake-linter-lib.walkFlake ./.));

        linter = flake-linter-lib.makeFlakeLinter {
          root = ./.;
          settings = {
            markdownlint = {
              paths = paths.markdown;
              settings = {
                MD013 = false;
                MD033 = false;
              };
            };

            nixpkgs-fmt.paths = paths.nix;
            prettier.paths = paths.markdown;
          };
        };
      in
      {
        checks = {
          flake-linter = linter.check;
        };

        apps = {
          inherit (linter) fix;
        };

        packages = {
          emacs = pkgs.callPackage ./home/programs/emacs/package {
            ggt = inputs.kira-nur.packages.${system}.ggt;
          };
        } // builtins.foldl'
          (packages: hostName:
            let hostModule = hosts.${hostName}; in
            packages // {
              "${hostName}/install-iso" = nixos-generators.nixosGenerate {
                inherit system;
                format = "install-iso";
                specialArgs = { inherit inputs; };
                modules = [ hostModule ./environments/install-iso.nix ];
              };
              "${hostName}/vm" = nixos-generators.nixosGenerate {
                inherit system;
                format = "vm";
                specialArgs = { inherit inputs; };
                modules = [ hostModule ./environments/vm.nix ];
              };
            })
          { }
          (builtins.attrNames hosts);
      });
}
