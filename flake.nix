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

    home-manager-unstable = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    kira-nur = {
      url = "gitlab:kira-bruneau/nur-packages";
      inputs = {
        flake-linter.follows = "flake-linter";
        flake-utils.follows = "flake-utils";
      };
    };

    jovian = {
      url = "github:Jovian-Experiments/Jovian-NixOS";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = { flake-utils, flake-linter, nixpkgs, ... } @ inputs:
    let
      lib = nixpkgs.lib;
      hosts = builtins.listToAttrs
        (builtins.map
          (file:
            let name = lib.removeSuffix ".nix" file; in {
              inherit name;
              value = {
                inputs = {
                  steamdeck = inputs // {
                    nixpkgs = inputs.nixpkgs-unstable;
                    home-manager = inputs.home-manager-unstable;
                  };
                }.${name} or inputs;

                module = {
                  imports = [ ./hosts/${file} ];
                  networking.hostName = name;
                };

                hardwareModule =
                  if builtins.pathExists ./hardware/hosts/${name}
                  then ./hardware/hosts/${name}
                  else null;
              };
            })
          (builtins.attrNames (builtins.readDir ./hosts)));
    in
    {
      nixosConfigurations = builtins.listToAttrs
        (builtins.concatMap
          (name:
            let host = hosts.${name}; in
            if host.hardwareModule != null
            then [
              {
                inherit name;
                value = host.inputs.nixpkgs.lib.nixosSystem {
                  specialArgs = { inherit (host) inputs; };
                  modules = [ host.hardwareModule host.module ];
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
          (packages: name:
            let
              host = hosts.${name};
              nixosGenerate = output: attrs: (host.inputs.nixpkgs.lib.nixosSystem (attrs // {
                inherit system;
                specialArgs = { inherit (host) inputs; };
                modules = [ host.module ] ++ attrs.modules;
              })).config.system.build.${output};
            in
            packages // {
              "${name}/install-iso" = nixosGenerate "isoImage" {
                modules = [ ./environments/install-iso.nix ];
              };
              "${name}/vm" = nixosGenerate "vm" {
                modules = [ ./environments/vm.nix ];
              };
            })
          { }
          (builtins.attrNames hosts);
      });
}
