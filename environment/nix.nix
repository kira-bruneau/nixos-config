{ inputs, ... }:

{
  nix = {
    # Pin nixpkgs in flake registry
    registry.nixpkgs.flake = inputs.nixpkgs;

    # Pin nixpkgs channel (for backwards compatibility with nix2 cli)
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

    settings = {
      auto-optimise-store = true;
      experimental-features = [ "nix-command" "flakes" ];
      keep-going = true;
    };
  };

  programs.git.enable = true;
}
