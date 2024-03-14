{ inputs, pkgs, pkgsDisco, ... }:

{
  imports = [
    inputs.disko.nixosModules.default
    ../services/ssh.nix
    ../services/syncthing.nix
    ../services/tailscale.nix
  ];

  environment.systemPackages = [
    pkgsDisco.default
  ];
}
