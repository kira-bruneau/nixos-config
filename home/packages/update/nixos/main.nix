{
  writeShellApplication,
  git,
  nix,
  nixos-rebuild,
}:

writeShellApplication {
  name = "update-nixos-main";

  runtimeInputs = [
    git
    nix
    nixos-rebuild
  ];

  text = ''
    git rebase update
    nixos-rebuild build --flake .
  '';
}
