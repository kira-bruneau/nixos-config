{
  description = "My home-manager configuration";

  inputs = {
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

    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, emacs-overlay, my-nur, flake-utils, flake-linter, nixpkgs }: {
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

      paths = flake-linter.lib.partitionToAttrs
        flake-linter.lib.commonPaths
        (builtins.filter
          (path:
            (builtins.all
              (ignore: !(nixpkgs.lib.hasSuffix ignore path))
              [
                "node-composition.nix"
                "node-env.nix"
                "node-packages.nix"
              ]))
          (flake-linter.lib.walkFlake ./.));

      linter = flake-linter.lib.makeFlakeLinter {
        root = ./.;

        settings = {
          markdownlint.paths = paths.markdown;
          nixpkgs-fmt.paths = paths.nix;
        };

        inherit pkgs;
      };
    in
    {
      apps = {
        inherit (linter) fix;
      };
    }
  );
}
