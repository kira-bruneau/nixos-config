{ inputs, pkgs, ... }:

{
  imports = [
    inputs.disko.nixosModules.disko
    inputs.kira-nur.nixosModules.overlay
    ../cachix.nix
    ../service/dnscrypt.nix
    ../service/ssh.nix
    ../service/tailscale.nix
    ./custom-nixos-generate-config.nix
    ./locale
    ./nix.nix
  ];

  environment.systemPackages = [
    inputs.disko.packages.${pkgs.stdenv.system}.default
  ];
}
