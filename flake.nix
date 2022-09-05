{
  description = "My home-manager configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-checker = {
      url = "gitlab:kira-bruneau/flake-checker";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    my-nur = {
      url = "gitlab:kira-bruneau/nur-packages";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
        flake-checker.follows = "flake-checker";
      };
    };
  };

  outputs = { self, flake-utils, nixpkgs, flake-checker, emacs-overlay, my-nur }: {
    nixosModules = {
      atlantis = { pkgs, ... }: {
        imports = [
          { _module.args.self = self; }
          ./host/atlantis.nix
        ];
        nixpkgs.overlays = [
          emacs-overlay.overlay
          my-nur.overlays.default
        ];
      };
      framework = { pkgs, ... }: {
        imports = [
          { _module.args.self = self; }
          ./host/framework.nix
        ];
        nixpkgs.overlays = [
          emacs-overlay.overlay
          my-nur.overlays.default
        ];
      };
    };
  } // flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};

      paths = flake-checker.lib.partitionToAttrs
        flake-checker.lib.commonFlakePaths
        (flake-checker.lib.walkFlake ./.);

      checker = flake-checker.lib.makeFlakeChecker {
        root = ./.;

        settings = {
          markdownlint.paths = paths.markdown;

          nixpkgs-fmt.paths = (builtins.filter
            (path:
              (builtins.all
                (ignore: !(nixpkgs.lib.hasSuffix ignore path))
                [
                  "node-composition.nix"
                  "node-env.nix"
                  "node-packages.nix"
                ]))
            paths.nix);

          prettier.paths = paths.markdown;
        };

        inherit pkgs;
      };
    in
    {
      apps = {
        inherit (checker) fix;
      };
    }
  );
}
