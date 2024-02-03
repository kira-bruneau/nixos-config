{ inputs, config, pkgs, pkgsUnstable, pkgsKiraNur, ... }:

{
  imports = [
    ../cachix.nix
    ../services/dnscrypt.nix
    ../services/ssh.nix
    ../services/tailscale.nix
    ./custom-nixos-generate-config.nix
    ./locale
    ./nix.nix
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.default
  ];

  _module.args = {
    pkgsKiraNur = inputs.kira-nur.packages.${pkgs.system};
    pkgsUnstable = import inputs.nixpkgs-unstable {
      system = pkgs.system;
      config = config.nixpkgs.config;
    };
  };

  home-manager = {
    useUserPackages = true;
    extraSpecialArgs = { inherit inputs pkgsUnstable pkgsKiraNur; };
  };

  environment.systemPackages = [
    inputs.disko.packages.${pkgs.system}.default
  ];
}
