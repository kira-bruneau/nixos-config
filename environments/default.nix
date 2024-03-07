{ inputs, config, pkgs, pkgsUnstable, pkgsKiraNur, pkgsOllama, ... }:

{
  imports = [
    inputs.home-manager.nixosModules.default
    ../cachix.nix
    ../services/dnscrypt.nix
    ./custom-nixos-generate-config.nix
    ./locale
    ./nix.nix
  ];

  _module.args = {
    pkgsKiraNur = inputs.kira-nur.packages.${pkgs.system};
    pkgsUnstable = import inputs.nixpkgs-unstable {
      system = pkgs.system;
      config = config.nixpkgs.config;
    };

    pkgsOllama = import inputs.nixpkgs-ollama {
      system = pkgs.system;
      config = config.nixpkgs.config;
    };
  };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = { inherit inputs pkgsUnstable pkgsKiraNur pkgsOllama; };
  };
}
