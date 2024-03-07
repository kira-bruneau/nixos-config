{ inputs, pkgs, ... }:

{
  imports = [
    inputs.disko.nixosModules.default
    ../services/ssh.nix
    ../services/syncthing.nix
    ../services/tailscale.nix
  ];

  environment.systemPackages = [
    inputs.disko.packages.${pkgs.system}.default
  ];
}
