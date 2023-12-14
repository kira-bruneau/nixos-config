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
    in
    {
      nixosModules = builtins.mapAttrs
        (host: path: {
          imports = [
            ({ _module.args = { inherit inputs; }; })
            path
          ];
        })
        hosts;
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        flake-linter-lib = flake-linter.lib.${system};

        paths = flake-linter-lib.partitionToAttrs
          flake-linter-lib.commonPaths
          (flake-linter-lib.walkFlake ./.);

        linter = flake-linter-lib.makeFlakeLinter {
          root = ./.;
          settings = {
            markdownlint.paths = paths.markdown;
            nixpkgs-fmt.paths = paths.nix;
            prettier.paths = paths.markdown;
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
