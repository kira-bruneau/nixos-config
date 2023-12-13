{ inputs, ... }:

{
  imports = [
    inputs.disko.nixosModules.disko
    inputs.kira-nur.nixosModules.overlay
    ../cachix.nix
    ../service/ssh.nix
    ./custom-nixos-generate-config.nix
    ./distributed-nix.nix
    ./locale
    ./nix.nix
  ];
}
