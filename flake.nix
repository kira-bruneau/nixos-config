{
  description = "My home-manager configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    my-nur = {
      url = "gitlab:kira-bruneau/nur-packages";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { self, flake-utils, nixpkgs, emacs-overlay, my-nur }: {
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
  };
}
