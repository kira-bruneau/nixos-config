{
  description = "Emacs wrapped with my custom environment";

  inputs =  {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    my-nur = {
      url = "github:kira-bruneau/nur-packages";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { self, flake-utils, nixpkgs, emacs-overlay, my-nur }:
    let
      applyOverlays = pkgs:
        let
          withEmacsOverlay = pkgs // (emacs-overlay.overlay final pkgs);
          final = withEmacsOverlay // (my-nur.overlay final withEmacsOverlay);
        in final;
    in
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
    ] (system:
      rec {
        defaultPackage = import ./. {
          pkgs = applyOverlays nixpkgs.legacyPackages.${system};
        };

        defaultApp = flake-utils.lib.mkApp {
          drv = defaultPackage;
          name = "emacs";
        };
      }
    );
}
