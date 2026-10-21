{
  description = "My NixOS configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    flake-linter = {
      url = "gitlab:kira-bruneau/flake-linter";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence = {
      url = "github:nix-community/impermanence";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
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

    nix-minecraft = {
      url = "github:Infinidoge/nix-minecraft";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs =
    {
      self,
      flake-utils,
      flake-linter,
      ...
    }@inputs:
    let
      lib = inputs.nixpkgs.lib;

      nixpkgsConfig = import ./nixpkgs-config.nix {
        inherit lib;
      };

      hostInputs = {
        peridot = inputs // {
          nixpkgs = inputs.jovian.inputs.nixpkgs;
          home-manager = inputs.home-manager-jovian;
        };
      };

      hosts = builtins.mapAttrs (
        name: value:
        if builtins.pathExists ./hosts/${name}/default.nix then
          let
            args = {
              inputs = hostInputs.${name} or inputs;
            };
          in
          {
            inherit name args;

            module =
              { pkgs, ... }:
              let
                system = pkgs.stdenv.hostPlatform.system;

                systemArgs = {
                  # For some reason, not manually unpacking causes an
                  # infinite recursion error, even though the
                  # resulting set is equivalent. Is this a Lix/Nix bug?
                  inherit ((self.legacyPackages.${system}.scope.overrideScope (final: prev: args)).args)
                    pkgsUnstable
                    pkgsCustom
                    pkgsDisko
                    pkgsNixIndexDatabase
                    pkgsKiraNur
                    pkgsNixMinecraft
                    ;
                };
              in
              {
                imports = [
                  {
                    networking.hostName = name;
                    _module.args = systemArgs;
                    home-manager.extraSpecialArgs = systemArgs;
                    nixpkgs.config = nixpkgsConfig;
                  }
                  ./environments/default.nix
                  ./hosts/${name}/default.nix
                ];
              };

            hardwareModule = {
              imports = [
                ./hardware/environments/default.nix
              ]
              ++ lib.optional (builtins.pathExists ./hosts/${name}/hardware.nix) ./hosts/${name}/hardware.nix
              ++ lib.optional (builtins.pathExists ./hosts/${name}/generated.nix) ./hosts/${name}/generated.nix
              ++ sharedModules;
            };
          }
        else
          null
      ) (builtins.readDir ./hosts);

      sharedModules = builtins.concatMap (
        name: lib.optional (builtins.pathExists ./hosts/${name}/shared.nix) ./hosts/${name}/shared.nix
      ) (builtins.attrNames hosts);
    in
    {
      nixosConfigurations = builtins.listToAttrs (
        builtins.concatMap (
          host:
          lib.optional (host != null && builtins.pathExists ./hosts/${host.name}/hardware.nix) (
            lib.nameValuePair host.name (
              lib.nixosSystem {
                specialArgs = host.args;
                modules = [
                  host.hardwareModule
                  host.module
                ];
              }
            )
          )
        ) (builtins.attrValues hosts)
      );
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        scope = lib.makeScope lib.callPackageWith (final: {
          inherit inputs;

          pkgs = import final.inputs.nixpkgs {
            inherit system;
            config = nixpkgsConfig;
          };

          args = {
            pkgsUnstable = import final.inputs.nixpkgs-unstable {
              inherit system;
              config = nixpkgsConfig;
            };

            pkgsCustom = lib.packagesFromDirectoryRecursive {
              inherit
                (lib.makeScope (final.pkgs.newScope) (self: {
                  fd = final.args.pkgsKiraNur.fd-relative-full-path;

                  nix =
                    (import ./environments/lix.nix (
                      {
                        inherit (final) inputs;
                        config = { };
                        inherit (final.nixpkgs) lib;
                      }
                      // final.args
                    )).nix.package;

                  nixos-rebuild = final.pkgs.nixos-rebuild.override {
                    nix = self.nix;
                  };

                  swaylock = final.args.pkgsKiraNur.swaylock-fprintd;
                }))
                callPackage
                newScope
                ;

              directory = ./home/packages;
            };

            pkgsDisko = final.inputs.disko.packages.${system};

            pkgsNixIndexDatabase = final.inputs.nix-index-database.packages.${system};

            pkgsKiraNur = final.inputs.kira-nur.packages.${system};

            pkgsNixMinecraft = final.inputs.nix-minecraft.legacyPackages.${system};
          };
        });

        scopeUnstable = scope.overrideScope (
          final: prev: {
            inputs = {
              pkgs = prev.inputs.pkgsUnstable;
            }
            // prev.inputs;
          }
        );

        flake-linter-lib = flake-linter.lib.${system};

        paths = flake-linter-lib.partitionToAttrs flake-linter-lib.commonPaths (
          builtins.filter (
            p:
            (builtins.all (ignore: !(lib.hasSuffix ignore p)) [
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

            nixfmt.paths = paths.nix;

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

        devShells.default = scope.pkgs.mkShell {
          nativeBuildInputs = linter.nativeBuildInputs;
        };

        legacyPackages = {
          inherit scope;
          unstable = lib.dontRecurseIntoAttrs scopeUnstable.args.pkgsCustom;
        }
        // scope.args.pkgsCustom
        // builtins.mapAttrs (
          name: host:
          if host != null then
            let
              nixosGenerate =
                output: attrs:
                (lib.nixosSystem (
                  attrs
                  // {
                    inherit system;
                    specialArgs = host.args;
                    modules = [ host.module ] ++ attrs.modules;
                  }
                )).config.system.build.${output};
            in
            lib.recurseIntoAttrs {
              "digital-ocean-image" = nixosGenerate "digitalOceanImage" {
                modules = [
                  host.hardwareModule
                  ./environments/digital-ocean-image.nix
                ];
              };
              "install-iso" = nixosGenerate "isoImage" {
                modules = [
                  host.hardwareModule
                  ./environments/install-iso.nix
                ];
              };
              "vm" = nixosGenerate "vm" {
                modules = [ ./environments/vm.nix ];
              };
            }
          else
            null
        ) hosts;
      }
    );
}
