{
  lib,
  writeShellApplication,
  git,
  nix,
  nixos-rebuild,
}:

writeShellApplication {
  name = "update-nixos-peridot";
  text = ''
    export PATH=${
      lib.makeBinPath [
        git
        nix
        nixos-rebuild
      ]
    }

    git rebase unstable
    nixos-rebuild build --flake .#peridot
  '';
}
