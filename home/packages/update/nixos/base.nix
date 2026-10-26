{
  writeShellApplication,
  git,
  nix,
  nixos-rebuild,
}:

writeShellApplication {
  name = "update-nixos";

  runtimeInputs = [
    git
    nix
    nixos-rebuild
  ];

  text = ''
    cleanup() {
        local exit_code changes
        exit_code=$?
        changes="$(git rev-parse HEAD)"
        git reset --hard '@{-1}'
        git switch -
        if [ "$exit_code" -eq 0 ]; then
            git merge --ff-only "$changes"
        fi
    }

    git switch --detach HEAD
    git pull --rebase all main
    trap cleanup EXIT
    nix flake update --commit-lock-file --option commit-lockfile-summary "flake.lock: update"
    nixos-rebuild build --flake .
  '';
}
