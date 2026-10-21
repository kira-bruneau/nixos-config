{
  writeShellApplication,
  git,
  nix,
}:

writeShellApplication {
  name = "update-nur";
  runtimeInputs = [
    git
    nix
  ];
  text = ''
    nix run .#sync ~/Dev/nixpkgs/nur
  '';
}
