{
  description = "My home-manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    my-nur = {
      url = "gitlab:kira-bruneau/nur-packages";
      inputs = {
        flake-utils.follows = "flake-utils";
        flake-linter.follows = "flake-linter";
        nixpkgs.follows = "nixpkgs";
      };
    };

    flake-utils.url = "github:numtide/flake-utils";

    flake-linter = {
      url = "gitlab:kira-bruneau/flake-linter";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/338da822b1506c26c18e6fced9527a8de4f08665";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { nixpkgs, my-nur, flake-utils, flake-linter, ... } @ inputs:
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
        ./environment/seed-home-config.nix
        {
          nixpkgs.overlays = [
            my-nur.overlays.default
          ];

          _module.args = { inherit inputs; };
        }
      ];
    in
    {
      nixosModules = builtins.mapAttrs
        (host: path: { imports = commonModules ++ [ path ]; })
        hosts;
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        flake-linter-lib = flake-linter.lib.${system};

        paths = flake-linter-lib.partitionToAttrs
          flake-linter-lib.commonPaths
          (builtins.filter
            (path:
              (builtins.all
                (ignore: !(lib.hasSuffix ignore path))
                [
                  "node-composition.nix"
                  "node-env.nix"
                  "node-packages.nix"
                ]))
            (flake-linter-lib.walkFlake ./.));

        linter = flake-linter-lib.makeFlakeLinter {
          root = ./.;
          settings = {
            markdownlint.paths = paths.markdown;
            nixpkgs-fmt.paths = paths.nix;
          };
        };
      in
      {
        apps = {
          inherit (linter) fix;
        };
      }
    );
}
