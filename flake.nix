{
  description = "My home-manager configuration";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    my-nur.url = "gitlab:kira-bruneau/nur-packages";
  };

  outputs = { self, emacs-overlay, my-nur }: {
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
