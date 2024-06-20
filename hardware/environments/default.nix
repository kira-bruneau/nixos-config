{
  inputs,
  pkgs,
  pkgsDisko,
  ...
}:

{
  imports = [
    inputs.disko.nixosModules.default
    ../services/ssh.nix
    ../services/syncthing.nix
    ../services/tailscale.nix
    ./impermanence.nix
  ];

  environment.systemPackages = with pkgs; [
    pciutils
    pkgsDisko.default
    smartmontools
  ];
}
