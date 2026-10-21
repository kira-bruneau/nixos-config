{
  writeShellApplication,
  coreutils,
  git,
  nix,
}:

writeShellApplication {
  name = "update-nixos-unstable";

  runtimeInputs = [
    coreutils
    git
    nix
  ];

  text = ''
    if ! git rebase update; then
      rm flake.lock
      nix flake lock
      git add flake.lock
      GIT_EDITOR=true git rebase --continue
    fi
  '';
}
