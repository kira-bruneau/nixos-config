{
  description = "My NixOS configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    flake-linter = {
      url = "gitlab:kira-bruneau/flake-linter";
      inputs.flake-utils.follows = "flake-utils";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence.url = "github:nix-community/impermanence";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager-jovian = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "jovian/nixpkgs";
    };

    kira-nur = {
      url = "gitlab:kira-bruneau/nur-packages";
      inputs = {
        flake-linter.follows = "flake-linter";
        flake-utils.follows = "flake-utils";
      };
    };

    jovian.url = "github:Jovian-Experiments/Jovian-NixOS";
  };

  outputs =
    { flake-utils, flake-linter, ... }@inputs:
    let
      lib = inputs.nixpkgs.lib;

      hosts = builtins.map (
        file:
        let
          name = lib.removeSuffix ".nix" file;
        in
        {
          inherit name;

          inputs =
            {
              peridot = inputs // {
                nixpkgs = inputs.jovian.inputs.nixpkgs;
                home-manager = inputs.home-manager-jovian;
              };
            }
            .${name} or inputs;

          module =
            {
              inputs,
              config,
              pkgs,
              ...
            }:
            let
              extraSpecialArgs = {
                pkgsUnstable = import inputs.nixpkgs-unstable {
                  system = pkgs.system;
                  config = config.nixpkgs.config;
                };

                pkgsDisko = inputs.disko.packages.${pkgs.system};

                pkgsNixIndexDatabase = inputs.nix-index-database.packages.${pkgs.system};

                pkgsKiraNur = inputs.kira-nur.legacyPackages.${pkgs.system};
              };
            in
            {
              imports = [
                ./environments/default.nix
                ./hosts/${file}
              ];

              networking.hostName = name;
              _module.args = extraSpecialArgs;
              home-manager.extraSpecialArgs = extraSpecialArgs;
              nixpkgs.overlays = [ (final: prev: { emacs = inputs.self.packages.${pkgs.system}.emacs; }) ];
            };

          hardwareModule = {
            imports =
              [ ./hardware/environments/default.nix ]
              ++ lib.optional (builtins.pathExists ./hardware/hosts/${name}/default.nix) ./hardware/hosts/${name}/default.nix
              ++ lib.optional (builtins.pathExists ./hardware/hosts/${name}/generated.nix) ./hardware/hosts/${name}/generated.nix
              ++ builtins.concatMap (host: host.sharedModules) hosts;
          };

          sharedModules = lib.optional (builtins.pathExists ./hardware/hosts/${name}/shared.nix) ./hardware/hosts/${name}/shared.nix;
        }
      ) (builtins.attrNames (builtins.readDir ./hosts));
    in
    {
      nixosConfigurations = builtins.listToAttrs (
        builtins.concatMap (
          host:
          if builtins.pathExists ./hardware/hosts/${host.name}/default.nix then
            [
              {
                inherit (host) name;
                value = host.inputs.nixpkgs.lib.nixosSystem {
                  specialArgs = {
                    inherit (host) inputs;
                  };
                  modules = [
                    host.hardwareModule
                    host.module
                  ];
                };
              }
            ]
          else
            [ ]
        ) hosts
      );
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          config = import ./nixpkgs-config.nix { inherit lib; };
        };

        pkgsKiraNur = inputs.kira-nur.legacyPackages.${pkgs.system};

        flake-linter-lib = flake-linter.lib.${system};

        paths = flake-linter-lib.partitionToAttrs flake-linter-lib.commonPaths (
          builtins.filter (
            path:
            (builtins.all (ignore: !(lib.hasSuffix ignore path)) [
              "cachix.nix"
              "generated.nix"
            ])
          ) (flake-linter-lib.walkFlake ./.)
        );

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

            nixf-tidy-fix = {
              paths = paths.nix;
              settings = {
                variable-lookup = true;
              };
            };

            nixfmt-rfc-style.paths = paths.nix;

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

        devShells.default = pkgs.mkShell { nativeBuildInputs = linter.nativeBuildInputs; };

        packages =
          {
            emacs = pkgs.callPackage ./home/programs/emacs/package.nix {
              inherit inputs pkgsKiraNur;
            };
          }
          // builtins.foldl' (
            packages: host:
            let
              nixosGenerate =
                output: attrs:
                (host.inputs.nixpkgs.lib.nixosSystem (
                  attrs
                  // {
                    inherit system;
                    specialArgs = {
                      inherit (host) inputs;
                    };
                    modules = [ host.module ] ++ attrs.modules;
                  }
                )).config.system.build.${output};
            in
            packages
            // {
              "${host.name}/digital-ocean-image" = nixosGenerate "digitalOceanImage" {
                modules = [
                  host.hardwareModule
                  ./environments/digital-ocean-image.nix
                ];
              };
              "${host.name}/install-iso" = nixosGenerate "isoImage" {
                modules = [
                  host.hardwareModule
                  ./environments/install-iso.nix
                ];
              };
              "${host.name}/vm" = nixosGenerate "vm" { modules = [ ./environments/vm.nix ]; };
            }
          ) { } hosts;
      }
    );
}
