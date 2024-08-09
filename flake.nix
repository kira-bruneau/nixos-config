{
  description = "My NixOS configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    flake-linter = {
      url = "gitlab:kira-bruneau/flake-linter";
      inputs.flake-utils.follows = "flake-utils";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixpkgs-chromium.url = "github:NixOS/nixpkgs/2748d22b45a99fb2deafa5f11c7531c212b2cefa";

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
      url = "github:nix-community/home-manager/release-24.11";
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

    nix-software-center = {
      url = "github:snowfallorg/nix-software-center";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nix-minecraft = {
      url = "github:Infinidoge/nix-minecraft";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs =
    { flake-utils, flake-linter, ... }@inputs:
    let
      lib = inputs.nixpkgs.lib;
      hosts = builtins.listToAttrs (
        builtins.map (
          file:
          let
            name = lib.removeSuffix ".nix" file;
          in
          {
            inherit name;
            value = {
              inputs =
                {
                  steamdeck = inputs // {
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

                    pkgsChromium = import inputs.nixpkgs-chromium {
                      system = pkgs.system;
                      config = config.nixpkgs.config;
                    };

                    pkgsDisko = inputs.disko.packages.${pkgs.system};

                    pkgsNixIndexDatabase = inputs.nix-index-database.packages.${pkgs.system};

                    pkgsKiraNur = inputs.kira-nur.packages.${pkgs.system};

                    pkgsNixSoftwareCenter = inputs.nix-software-center.packages.${pkgs.system};

                    pkgsNixMinecraft = inputs.nix-minecraft.legacyPackages.${pkgs.system};
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

              hardwareModule =
                if builtins.pathExists ./hardware/hosts/${name} then
                  {
                    imports =
                      [ ./hardware/environments/default.nix ]
                      ++ lib.optional (builtins.pathExists ./hardware/hosts/${name}/default.nix) ./hardware/hosts/${name}/default.nix
                      ++ lib.optional (builtins.pathExists ./hardware/hosts/${name}/generated.nix) ./hardware/hosts/${name}/generated.nix;
                  }
                else
                  null;
            };
          }
        ) (builtins.attrNames (builtins.readDir ./hosts))
      );
    in
    {
      nixosConfigurations = builtins.listToAttrs (
        builtins.concatMap (
          name:
          let
            host = hosts.${name};
          in
          if host.hardwareModule != null then
            [
              {
                inherit name;
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
        ) (builtins.attrNames hosts)
      );
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          config.allowUnfreePredicate = import ./unfree.nix { inherit lib; };
        };

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
            emacs = pkgs.callPackage ./home/programs/emacs/package.nix { };
          }
          // builtins.foldl' (
            packages: name:
            let
              host = hosts.${name};
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
              "${name}/install-iso" = nixosGenerate "isoImage" {
                modules = [
                  ./environments/install-iso.nix
                ] ++ lib.optional (host.hardwareModule != null) host.hardwareModule;
              };
              "${name}/vm" = nixosGenerate "vm" { modules = [ ./environments/vm.nix ]; };
            }
          ) { } (builtins.attrNames hosts);
      }
    );
}
