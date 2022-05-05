{ self, pkgs, ... }:

{
  system.activationScripts.createNixOSConfigGitRepo =
    ''
    if [ ! -e /etc/nixos -o -z $(ls -A /etc/nixos) ]; then
      mkdir -p /etc
      cp -RT --no-preserve=ownership ${self} /etc/nixos
      chmod -R +w /etc/nixos
      cd /etc/nixos
      ${pkgs.git}/bin/git init -b main
      ${pkgs.git}/bin/git config include.path ../.gitconfig
    fi
  '';
}
