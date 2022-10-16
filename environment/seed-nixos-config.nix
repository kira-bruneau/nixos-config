{ inputs, pkgs, ... }:

{
  system.activationScripts.seedNixOSConfig = ''
    if [ -z "$(ls -A /etc/nixos 2>/dev/null)" ]; then
      mkdir -p /etc
      cp -RT --no-preserve=ownership ${inputs.self} /etc/nixos
      chmod -R +w /etc/nixos
      cd /etc/nixos
      ${pkgs.git}/bin/git init -b main
      ${pkgs.git}/bin/git config include.path ../.gitconfig
    fi
  '';
}
