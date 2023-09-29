{ config, pkgs, ... }:

{
  imports = [
    ../service/ssh.nix
  ];

  system.activationScripts.seedBinaryCacheKey = ''
    if ! [ -f /etc/nix/private-key ]; then
      ${pkgs.nix}/bin/nix-store --generate-binary-cache-key \
        ${config.networking.hostName}-1 \
        /etc/nix/private-key \
        /etc/nix/public-key
    fi
  '';
}
