{ inputs, config, lib, pkgs, pkgsUnstable, pkgsKiraNur, pkgsOllama, ... }:

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

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (builtins.parseDrvName (lib.getName pkg)).name [
    "anytype"
    "anytype-heart"
    "clonehero"
    "clonehero-unwrapped"
    "data.zip"
    "discord"
    "minecraft-server"
    "sm64ex"
    "steam"
    "steam-jupiter-original"
    "steam-original"
    "steam-run"
    "steamdeck-hw-theme"
    "unrar"
    "vvvvvv"
  ];
}
