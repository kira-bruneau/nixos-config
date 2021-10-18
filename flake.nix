{
  description = "My home-manager configuration";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    my-nur = {
      url = "gitlab:kira-bruneau/nur-packages";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    emacs-config = {
      url = "gitlab:kira-bruneau/emacs-config";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
        my-nur.follows = "my-nur";
      };
    };
  };

  outputs = { self, flake-utils, nixpkgs, my-nur, emacs-config }: {
    nixosModules = {
      atlantis = { pkgs, ... }: {
        imports = [ ./host/atlantis.nix ];
        nixpkgs.overlays = [
          my-nur.overlay
          (self: super: {
            emacs = emacs-config.defaultPackage.${super.stdenv.hostPlatform.system};
          })
        ];
      };
    };
  };
}
